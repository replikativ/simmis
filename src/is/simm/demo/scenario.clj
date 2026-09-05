(ns is.simm.demo.scenario
  "Load a whole simmis workspace from a declarative scenario.

   WHY THIS EXISTS, in order of value:

   1. An empty simmis reads as an UNFINISHED simmis. Drives, mail, schedules
      and the wiki are all complete features that render as blank panels with
      nothing in them, which is the worst possible misreading.
   2. Benchmarks need a world. Agent workflows can only be scored against a
      workspace that has enough in it to reason over, and that is the SAME
      workspace every run.
   3. Onboarding. A new tenant that starts populated is a product feature, not
      a demo prop — which is why this lives in `src` rather than `test`.

   EVERYTHING GOES THROUGH THE OPS LAYER. `create-kb!`, `create-drive!`,
   `create-room!` — never raw datoms. A seeder that wrote datoms directly would
   have cheerfully produced a beautiful workspace on top of every write-path bug
   we found this week (an uninstalled `:S.Page/kind`, a diff comparing eids
   across branches, an authorization check that threw). Going through the real
   functions means seeding FAILS when the product is broken, so it doubles as a
   smoke test and cannot drift from reality as the schema evolves.

   DETERMINISM. Page and block uuids are DERIVED from their scenario identity
   (`hasch/uuid`), and instants come from the scenario rather than `(Date.)`.
   Re-running produces the same content ids, so assertions can name entities and
   two runs are comparable. Store ids are still minted randomly by `create-kb!`
   and friends — content-level determinism is what benchmarks need, and stores
   are found by name.

   NARRATIVE TIME ON THE TRANSACTION, not just on the row. Every wiki write
   carries the scenario's instant and its author in `:tx-meta` (see
   `provenance`), so a seeded workspace has a real history rather than a
   plausible-looking present: the Timelines rail spreads across the months the
   scenario describes, `d/as-of` at any of them answers, and the audit panel can
   name who made each change.

   AND THE STORES THEMSELVES ARE INSTALLED THERE (`earliest-inst`). Back-dating
   only the content leaves each store's schema and seed stamped with the moment
   it was provisioned — younger than everything in it — so a cut before that
   moment sits in a database that has content but no vocabulary, where a query
   naming a seed entity through a lookup ref throws instead of returning
   nothing. The rail exists to be scrubbed to the left; the left has to be a
   place the workspace can be read from.

   Two seeded write paths still land at wall clock, both because the function
   that owns the transaction takes no `:tx-meta`: chat messages
   (`room-agents/persist-message!`) and book entries (kontor posts through its
   validation gate). Neither is on the rail — that reads KB replicas only — and
   both already carry their narrative time and their author ON the row
   (`:S.Message/sent-at` + `:S.Message/author`, `:kontor.transaction/…`), so
   what is missing is transaction-level provenance in the ROOM stores. Fixing it
   means an optional `:tx-meta` argument on those two functions."
  (:require [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.drives :as drives]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.parties :as parties]
            [is.simm.model.seed :as seed]
            [is.simm.agents.room-agents :as ra]
            [is.simm.model.fractional-index :as frac]
            [is.simm.ops.proposals :as proposals]
            [kontor.book :as book]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [muschel.fs :as mfs]
            [org.replikativ.spindel.engine.core :as rtc]
            [clojure.edn :as edn]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [taoensso.telemere :as log]))

;; ---------------------------------------------------------------------------
;; Deterministic identity + time
;; ---------------------------------------------------------------------------

(defn ident-uuid
  "A uuid derived from what the thing IS, so a re-seed reproduces it. Benchmarks
   assert on these; a `random-uuid` here would make every run incomparable."
  [& parts]
  (hasch/uuid (vec parts)))

(defn- inst
  "Scenario timestamps are explicit ISO-8601 strings. Never `(Date.)` — a
   wall-clock seed makes the timeline different on every run, which breaks both
   time-travel demos and any assertion about ordering."
  [s]
  (java.util.Date/from (java.time.Instant/parse s)))

(defn- provenance
  "`:tx-meta` for a seeded write: WHEN it happened and WHO did it.

   Datahike auto-stamps `:db/txInstant` and a value supplied in `:tx-meta` wins
   over the allocator (`datahike.db.transaction/next-tx-instant`), so a write
   lands at its narrative instant without any entity having to carry a second
   copy of it. This is the difference between a seeded workspace that HAS a
   history and one that merely contains old-looking prose: the Timelines rail
   reads `:db/txInstant`, and a seed that writes a whole company inside one
   minute put 148 of its 149 dots inside the leftmost 5% of the axis — a smudge
   with nothing to scrub to.

   `:tx/author` (schema.clj `provenance-schema`) is the party uuid as a STRING,
   not a ref: these transactions land in per-KB stores while parties live in the
   shared system DB, so a ref would dangle. It is what turns the audit panel
   from `this page changed` into `Vár changed this page`."
  [when-> author-id]
  (cond-> {}
    when->    (assoc :db/txInstant when->)
    author-id (assoc :tx/author (str author-id))))

(defn- earliest-inst
  "The first moment the scenario claims anything happened, found by scanning
   EVERY `:at` in it rather than by naming the sections that carry one — a
   scenario that grows a new dated section must not silently move the beginning
   of time out from under the stores.

   It is what the stores are installed at (see `load-kb!`). Equal to the first
   content instant rather than a moment before it, deliberately: `d/as-of`
   filters on the instant, so install and first page share a cut and there is no
   window between them for a query to fall into. A subtracted margin would be an
   arbitrary number pretending to be a fact."
  [s]
  (->> (tree-seq coll? seq s)
       (keep #(when (map? %) (:at %)))
       (keep #(try (inst %) (catch Exception _ nil)))
       sort
       first))

(defn- chronological
  "Scenario items in narrative order rather than file order.

   Backdating is per transaction and datahike does not police the sequence.
   Measured in a scratch in-memory db: a transaction stamped two months BEFORE
   the one written ahead of it is accepted silently, no exception, no warning.
   The cost surfaces later and quietly — `d/since` at a cut between the two
   returns NOTHING (it resolves a cut to a tx id, and the tx ids no longer agree
   with the instants), and `d/history` shows a value being retracted two months
   before it was asserted. `d/as-of` is the one operation that stays honest
   under out-of-order stamps, because it filters on the instant itself.

   So the seeder sorts, and the invariant is structural instead of a comment
   nobody re-reads while adding a page to the scenario."
  [items]
  (vec (sort-by #(some-> (:at %) inst) items)))

;; ---------------------------------------------------------------------------
;; Wiki pages
;; ---------------------------------------------------------------------------

(defn- add-page!
  "One page and its blocks, at the scenario's instant and attributed to
   `author-id`.

   `blocks` are HTML strings; a block may instead be `{:html … :viz {…}}` to
   carry a Vega-Lite spec, which the block editor renders as a chart.

   Wikilinks are NOT resolved here — `link-page!` does that once every page in
   the KB exists, and the reason is in its docstring."
  [kb-conn kb-name author-id {:keys [title blocks at]}]
  (let [page-uuid (ident-uuid :page kb-name title)
        when-> (inst at)
        meta-> (provenance when-> author-id)
        blocks (vec blocks)
        orders (rest (reductions (fn [prev _] (frac/generate-key-between prev nil))
                                 nil blocks))]
    (d/transact kb-conn
                {:tx-data [{:entity/uuid page-uuid
                            :entity/name title
                            :entity/created-at when-> :entity/updated-at when->
                            :instance/of-role [:entity/name "S/Page"]
                            :S.Page/title title
                            :S.Page/archived false}]
                 :tx-meta meta->})
    (d/transact kb-conn
                {:tx-data (mapv (fn [b order i]
                                  (let [html (if (map? b) (:html b) b)]
                                    (cond-> {:entity/uuid (ident-uuid :block kb-name title i)
                                             :entity/created-at when-> :entity/updated-at when->
                                             :instance/of-role [:entity/name "S/Block"]
                                             :block/parent [:entity/uuid page-uuid]
                                             :block/order order
                                             :block/content html}
                                      (and (map? b) (:viz b))
                                      (assoc :block/viz-spec (pr-str (:viz b))))))
                                blocks orders (range))
                 :tx-meta meta->})
    page-uuid))

(defn- link-page!
  "Turn this page's [[Title]] into stored refs, so backlinks and
   wiki/neighborhood work on seeded content exactly as on typed content.

   A SECOND PASS, after every page in the KB exists, because
   `link-block-references!` has Roam semantics: a link to a title it cannot find
   CREATES that page, with a placeholder empty block (the editor treats a
   blockless page as still loading). The scenario links forward constantly — the
   first Handbook page points at four pages seeded after it — so linking as each
   page landed minted a stub for every forward link.

   No duplicate page came of it: `:entity/name` is unique identity, so
   `add-page!` upserted onto the stub. What it left behind was the stub's empty
   block, sitting ABOVE the page's real first block, and a transaction stamped
   `(Date.)` deep inside `link-block-references!` where no `:tx-meta` reaches.
   Measured on the Tröskel Handbook: 53 content transactions instead of 40, the
   13 extra ones all at wall clock — dots at the right edge of a rail this
   seeder exists to spread out. Linking last resolves both, because every title
   is already there and nothing is minted."
  [kb-conn kb-name {:keys [title blocks]}]
  (doseq [[b i] (map vector (vec blocks) (range))]
    (try
      (kbs/link-block-references! kb-conn (ident-uuid :block kb-name title i)
                                  (if (map? b) (:html b) b))
      (catch Exception e
        (log/log! {:level :warn :id ::link-refs-failed
                   :data {:page title :block i :error (.getMessage e)}})))))

(defn- load-kb!
  "One KB, installed at `t0` and filled with back-dated pages.

   `t0` is the whole SCENARIO's earliest instant, not this KB's — the rail is
   global and a reader scrubbing to the beginning of the story must find every
   wiki already in existence there, not each one blinking into being at its own
   first page. Without it the store's schema and seed are stamped `(Date.)`
   while its pages claim March, and a cut in between throws on any query that
   names a seed entity through a lookup ref (see `store/install!`)."
  [owner-id t0 {:keys [name pages]}]
  (let [kb (kbs/create-kb! owner-id name :at t0)
        conn (kbs/connect-kb-database (:kb/db-scope kb))
        ordered (chronological pages)]
    (doseq [p ordered] (add-page! conn name owner-id p))
    (doseq [p ordered] (link-page! conn name p))
    (log/log! {:level :info :id ::kb-seeded
               :data {:kb name :pages (count pages)}})
    (assoc kb :seeded/pages (count pages))))

;; ---------------------------------------------------------------------------
;; Drives
;; ---------------------------------------------------------------------------

(defn- load-drive!
  "`files` are `{:path \"finance/2026-q1.md\" :content \"…\"}`; the directory
   part is created on demand. Content goes through the real CAS, so seeded files
   are indistinguishable from uploaded ones — the agent's /drive mount reads
   them the same way."
  [owner-id {:keys [name files]}]
  (let [drive (drives/create-drive! owner-id name)
        conn (drives/connect-drive-database (:drive/db-scope drive))]
    (doseq [{:keys [path content mime]} files]
      (try
        (let [segs (vec (remove clojure.string/blank?
                                (clojure.string/split (str path) #"/")))
              dirs (vec (butlast segs))
              fname (last segs)
              parent (when (seq dirs) (drives/ensure-path! conn dirs))]
          (drives/put-file! conn parent fname
                            (.getBytes (str content) "UTF-8")
                            :mime (or mime "text/markdown")
                            :source :seed))
        (catch Exception e
          (log/log! {:level :warn :id ::drive-file-failed
                     :data {:drive name :path path :error (.getMessage e)}}))))
    (log/log! {:level :info :id ::drive-seeded
               :data {:drive name :files (count files)}})
    (assoc drive :seeded/files (count files))))

;; ---------------------------------------------------------------------------
;; Rooms + conversation history
;; ---------------------------------------------------------------------------

(defn- resolve-speaker
  "Map a scenario speaker to a real party.

   `:me`    — the workspace owner.
   `:agent` — THIS room's resident agent, passed in.
   other    — a literal `:party/handle`.

   `:agent` used to mean `(first (filter agent? (list-parties)))` — the first
   agent party in the whole workspace. Rooms were created with no participants
   at all, so there was nothing better to point at, and the effect was that
   every room's seeded agent lines were attributed to some other room's agent.
   Rooms now mint their own agent (see `load-room!`), which fixes the
   attribution and the membership together."
  [owner-id room-agent-id handle]
  (case handle
    :me owner-id
    :agent room-agent-id
    (some-> (parties/get-party-by-handle (name handle)) :party/id)))

(defn- seed-room-repo!
  "Write `files` into the ROOM's own git repo and commit them.

   A room's workspace is a geschichte repo held as content-addressed blobs in
   konserve, not a directory — `find` will not show these files and a fixture
   written into the shared `.dvergr/workspace` never reaches a room, which is
   how an agent came to report (correctly) that code it was asked to fix did
   not exist. The write goes through the same muschel MountFS the agent's own
   `read_file`/`write_file` tools use, so there is no separate seeding path to
   drift from what the agent sees.

   Committing matters as much as writing: the demo's coding lane opens by
   reading a file that is already part of the repo's history, and an uncommitted
   working-tree file would make `git log` disagree with what the agent found."
  [room files message]
  (let [gesch (requiring-resolve 'dvergr.substrate.geschichte/filesystem)
        git (requiring-resolve 'dvergr.substrate.geschichte/execute-git)
        room-obj ((requiring-resolve 'dvergr.room.registry/lookup) (:room/slug room))]
    (if-not room-obj
      (log/log! {:level :warn :id ::repo-room-not-registered
                 :msg "Room is not in the registry — repo files skipped"
                 :data {:room (:room/name room) :slug (:room/slug room)}})
      (binding [rtc/*execution-context* (:ctx room-obj)]
        (if-let [fs (gesch)]
          (do
            (doseq [{:keys [path content]} files]
              (try
                (let [segs (vec (remove str/blank? (str/split (str path) #"/")))
                      dirs (butlast segs)]
                  ;; parents first; `mkdir` throws rather than returning nil when
                  ;; the path is already there, so existence is checked not caught
                  (reduce (fn [acc d]
                            (let [p (str acc "/" d)]
                              (when-not (mfs/exists? fs p) (mfs/mkdir fs p))
                              p))
                          "" dirs)
                  (when-let [sink (mfs/open-sink fs (str "/" (str/join "/" segs)) false)]
                    (if (map? sink)
                      (do (when-let [w (:write! sink)]
                            (w (.getBytes ^String (str content) "UTF-8")))
                          (mfs/commit-sink! sink))
                      (with-open [os sink]
                        (.write ^java.io.OutputStream os
                                (.getBytes ^String (str content) "UTF-8"))))))
                (catch Exception e
                  (log/log! {:level :warn :id ::repo-file-failed
                             :data {:room (:room/name room) :path path
                                    :error (.getMessage e)}}))))
            (git ["add" "-A"])
            (git ["commit" "-m" message])
            (log/log! {:level :info :id ::repo-seeded
                       :data {:room (:room/name room) :files (count files)}}))
          (log/log! {:level :warn :id ::repo-no-filesystem
                     :msg "Room has no geschichte workspace — repo files skipped"
                     :data {:room (:room/name room)}}))))))

(defn- load-room!
  "A room with its conversation already in it, and its agent actually IN it.

   Messages are written through `persist-message!` with the SCENARIO's
   timestamps, so the timeline is the one the scenario describes rather than
   the moment of seeding.

   THE AGENT IS A MEMBER. Rooms used to be created with `#{}` participants, so
   a seeded workspace showed rooms whose only party was the owner: the agent
   appeared in the transcript but was not in the room, and asking it anything
   got no reply because nothing was there to answer. `:agent` in a room spec
   mints one through the same `create-agent!` + `add-party!` path the
   `add-agent-to-room!` RPC uses, so a seeded room is indistinguishable from
   one assembled by hand."
  [owner-id {:keys [name kind messages attach-drives attach-kbs agent agents repo]} ctx-idx]
  (let [room (rooms/create-room! owner-id name (or kind :project) #{})
        room-id (:room/id room)
        conn (room-dbs/connect-room-database (:room/content-db-scope room))
        ;; `:agents` (a vector) or `:agent` (one map). Four agents is the shape
        ;; the incident demo needs — one per lane, each with its own tool scope
        ;; in its prompt — and one is still the common case for an assistant
        ;; room, so both spellings stay.
        specs (cond agents (vec agents) agent [agent] :else [])
        made-agents
        (vec (for [spec specs]
               (let [display (or (:name spec) "Vár")
                     a (parties/create-agent!
                        owner-id
                        (cond-> {:display-name display
                                 ;; `create-agent!` does not derive one, and a
                                 ;; handle-less party cannot be @-mentioned.
                                 ;; Derived from ROOM + NAME so four agents in
                                 ;; one room get four distinct stable handles.
                                 :handle (str (str/lower-case display) "-"
                                              (subs (str (ident-uuid :room-agent name display)) 0 8))
                                 :auto-respond? true}
                          (:system-prompt spec)
                          (assoc :system-prompt (:system-prompt spec))))]
                 (rooms/add-party! room-id (:party/id a))
                 (log/log! {:level :info :id ::room-agent-seeded
                            :data {:room name :agent (:party/handle a)
                                   :role (:role spec)}})
                 a)))
        room-agent (first made-agents)
        speaker #(resolve-speaker owner-id (:party/id room-agent) %)]
    ;; `persist-message!` points `:S.Message/room` at a lookup ref, so the
    ;; S/ChatRoom entity has to exist in the room's OWN store before any
    ;; message can be written — creating the room does not put it there.
    (when-not (d/q '[:find ?e . :in $ ?u :where [?e :entity/uuid ?u]] @conn room-id)
      (d/transact conn [(seed/generate-chat-room room-id name)]))
    ;; every distinct speaker needs an author entity in the room store too
    (doseq [h (distinct (map :from messages))]
      (when-let [pid (speaker h)]
        (when-let [p (parties/get-party pid)]
          (ra/ensure-room-party-entity! conn p))))
    (doseq [[{:keys [from at text]} i] (map vector messages (range))]
      (if-let [pid (speaker from)]
        (ra/persist-message! conn
                             (ident-uuid :msg name i)
                             text
                             room-id
                             pid
                             nil
                             (inst at))
        (log/log! {:level :warn :id ::unknown-speaker
                   :msg "Scenario names a party that does not exist — message skipped"
                   :data {:room name :handle from :index i}})))
    ;; grants: which drives and wikis this room can see. Without these the
    ;; room's agents cannot read the workspace, and drives stay unbrowsable.
    (doseq [dn attach-drives]
      (when-let [d (first (filter #(= dn (:drive/name %))
                                  (drives/list-drives owner-id)))]
        (drives/attach-drive-to-room! room-id (:drive/id d))))
    (doseq [kn attach-kbs]
      (when-let [k (first (filter #(= kn (:kb/name %)) (kbs/get-party-kbs owner-id)))]
        (try (kbs/attach-kb-to-room! room-id (:kb/id k))
             (catch Exception e
               (log/log! {:level :warn :id ::kb-attach-failed
                          :data {:room name :kb kn :error (.getMessage e)}})))))
    ;; The room's own git repo, so a coding agent FINDS code rather than being
    ;; asked to invent it. Written through the same muschel mount the agent's
    ;; own file tools use — the repo is a geschichte repo held as
    ;; content-addressed blobs, so this is not a filesystem write and a fixture
    ;; dropped in `.dvergr/workspace` never reaches a room.
    (when (seq (:files repo))
      (seed-room-repo! room (:files repo) (or (:message repo) "seed workspace")))
    (log/log! {:level :info :id ::room-seeded
               :data {:room name :messages (count messages)
                      :agents (mapv :party/handle made-agents)
                      :repo-files (count (:files repo))}})
    (assoc room :seeded/messages (count messages)
                :seeded/agents (mapv :party/id made-agents)
                :seeded/agent (some-> room-agent :party/id))))

;; ---------------------------------------------------------------------------
;; Books — money that actually moved
;; ---------------------------------------------------------------------------

(defn- load-book!
  "Post a room's opening entries through `kontor.book/entry!`.

   Through the real verb, not raw datoms, for the reason in the ns docstring:
   the book is GOVERNED, so a seeder writing postings directly would sail past
   the balance invariant that every agent posting has to satisfy. Seeding the
   way an agent posts means a broken posting path fails here first.

   The commodity is the scenario's, not the starter book's. `starter-book`
   seeds USD so the verbs are reachable at all — that is a default, not a claim
   about the tenant — and a Swedish letting agency keeping its ledger in dollars
   would be a small lie told in the one view that exists to be exact.

   `:narration` is kontor's field for what a transaction WAS. `:description`
   looks right, is accepted without complaint, and is silently dropped.

   Journals are resolved by TYPE, never by code. A code is a tenant's naming
   choice — Tröskel's cash journal is `CR`, a survivor of the old CR/CD pair —
   while the type is what `kontor.book` resolves a verb against. An earlier
   version passed `[:kontor.journal/code \"CSH\"]` and two of six entries failed
   against a book that had a perfectly good cash journal under another name."
  [made-rooms {:keys [room commodity entries]}]
  (if-let [r (first (filter #(= room (:room/name %)) made-rooms))]
    (let [conn (room-dbs/connect-room-database (:room/content-db-scope r))
          {:keys [symbol name precision]} commodity]
      (d/transact conn [{:kontor.commodity/symbol symbol
                         :kontor.commodity/name name
                         :kontor.commodity/precision (or precision 2)}])
      (doseq [{:keys [at narration debit credit amount journal-type]} entries]
        (try
          (book/entry! conn
                       {:debit-account [:kontor.account/path debit]
                        :credit-account [:kontor.account/path credit]
                        :amount amount
                        :commodity [:kontor.commodity/symbol symbol]
                        :journal-type (or journal-type :general)
                        :effective-date (inst at)
                        :narration narration})
          (catch Exception e
            (log/log! {:level :warn :id ::book-entry-failed
                       :data {:room room :narration narration
                              :error (.getMessage e)}})
            ;; A scenario is also a production-path smoke test. Continuing
            ;; used to return `:book/entries (count entries)` after every post
            ;; had failed, giving the UI and benchmarks a plausible but empty
            ;; ledger. Preserve the local diagnostic and fail the load.
            (throw (ex-info "Scenario book entry failed"
                            {:room room :narration narration}
                            e)))))
      (log/log! {:level :info :id ::book-seeded
                 :data {:room room :entries (count entries) :commodity symbol}})
      {:book/room room :book/entries (count entries)})
    (log/log! {:level :warn :id ::book-skipped
               :msg "Scenario books a room that was not seeded"
               :data {:room room}})))

;; ---------------------------------------------------------------------------
;; Proposals — a branch with real edits on it, waiting for review
;; ---------------------------------------------------------------------------

(defn- apply-page-edit!
  "One page's worth of change ON A BRANCH.

   `:add-blocks` appends, `:edit-block` rewrites the block at an index, and
   `:remove-block` RETRACTS one. The retraction is the interesting case and the
   reason this exists: a proposal that only ever adds is a weaker claim than the
   product makes, because it never shows that withdrawing something propagates
   through review the same way adding it does.

   Blocks are addressed by their scenario INDEX rather than by content, since
   `add-page!` derives every block's uuid from `(kb-name, page-title, index)` —
   the same determinism that lets a benchmark name an entity lets a proposal
   name the block it is editing."
  [conn kb-name author-id {:keys [page add-blocks edit-block remove-block at]}]
  (let [page-uuid (ident-uuid :page kb-name page)
        block-uuid #(ident-uuid :block kb-name page %)
        when-> (inst at)
        meta-> (provenance when-> author-id)]
    (when-let [{:keys [index html]} edit-block]
      (d/transact conn {:tx-data [{:entity/uuid (block-uuid index)
                                   :block/content html
                                   :entity/updated-at when->}]
                        :tx-meta meta->}))
    (when-let [index remove-block]
      (d/transact conn {:tx-data [[:db/retractEntity [:entity/uuid (block-uuid index)]]]
                        :tx-meta meta->}))
    (when (seq add-blocks)
      ;; appended after the existing blocks, so order keys continue the page
      ;; rather than colliding with it
      (let [existing (d/q '[:find [?o ...] :in $ ?p
                            :where [?b :block/parent ?pe] [?pe :entity/uuid ?p]
                                   [?b :block/order ?o]]
                          @conn page-uuid)
            ;; `(last (sort …))`, not `max-key`: fractional-index keys are
            ;; STRINGS ordered lexicographically, and max-key wants a numeric
            ;; key fn — it would compare Strings as if they were magnitudes.
            start (last (sort existing))
            orders (rest (reductions (fn [prev _] (frac/generate-key-between prev nil))
                                     start add-blocks))]
        (d/transact conn
                    {:tx-data (mapv (fn [html order i]
                                      {:entity/uuid (ident-uuid :block kb-name page :proposed i)
                                       :entity/created-at when-> :entity/updated-at when->
                                       :instance/of-role [:entity/name "S/Block"]
                                       :block/parent [:entity/uuid page-uuid]
                                       :block/order order
                                       :block/content html})
                                    add-blocks orders (range))
                     :tx-meta meta->})))))

(defn- load-proposal!
  "A proposal filed against a seeded KB: branch it, edit ON the branch, file.

   `:room` is REQUIRED and resolved by name — `access/proposal-room-eid` returns
   nil without it, and then every write RPC on the proposal denies, so Accept
   fails at the last moment with an authorization error rather than at seed time
   with a clear one.

   The base commit is captured BEFORE branching. It is what the diff reads the
   `before` side out of, and taking it afterwards would silently record the
   branch's own first commit as the point it diverged from."
  [owner-id made-kbs made-rooms {:keys [title summary kb room intent edits new-pages]}]
  (let [k (first (filter #(= kb (:kb/name %)) made-kbs))
        r (first (filter #(= room (:room/name %)) made-rooms))]
    (if-not (and k r)
      (log/log! {:level :warn :id ::proposal-skipped
                 :msg "Scenario names a KB or room that was not seeded"
                 :data {:title title :kb kb :room room :kb? (some? k) :room? (some? r)}})
      (let [scope (:kb/db-scope k)
            base (branching/branch-head-id scope :db)
            ;; Deterministic, like every other id here: a re-seed of the same
            ;; scenario asks for the SAME branch and `branch-kb!` reports
            ;; `:existed? true` instead of accumulating near-duplicates.
            ;; `hash` would do too, but a negative hash reads as `proposal--12…`.
            {:keys [branch]} (branching/branch-kb!
                              scope (str "proposal-"
                                         (subs (str (ident-uuid :proposal kb title)) 0 8)))
            bconn (branching/get-kb-conn-on-branch scope branch)
            ;; New pages before edits regardless of file order, and each list in
            ;; narrative order — the branch's writes have to continue the trunk's
            ;; instants rather than fall behind them (see `chronological`), and
            ;; an edit that ran before the page it edits would be addressing a
            ;; block that does not exist yet.
            fresh (chronological new-pages)
            changed (chronological edits)]
        (doseq [p fresh] (add-page! bconn kb owner-id p))
        (doseq [p fresh] (link-page! bconn kb p))
        (doseq [e changed] (apply-page-edit! bconn kb owner-id e))
        (let [id (proposals/file-proposal!
                  {:title title :summary summary :intent intent
                   :author owner-id :room (:room/id r)
                   :forks [{:scope scope :branch branch
                            :base-commit base :system-type :kb}]})]
          (log/log! {:level :info :id ::proposal-seeded
                     :data {:title title :kb kb :branch branch :proposal id}})
          {:proposal/id id :proposal/title title :proposal/branch branch})))))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(defn read-scenario
  "Read a scenario from `resources/demo/<id>.edn`.

   Classpath first (`resources` is on `:paths` and copied into the uberjar),
   with a working-directory fallback for a REPL started outside the project
   root."
  [id]
  (let [rel (str "demo/" (name id) ".edn")
        src (or (io/resource rel)
                (let [f (io/file "resources" rel)] (when (.exists f) f)))]
    (some-> src slurp edn/read-string)))

(defn- load-customers!
  "Seed the incident's customer records into a named KB.

   `{:kb Customers :fixture :incident}` — the fixture is a separate file
   (`resources/demo/incident.edn`) because 46 accounts is data, not narrative,
   and it is shared with `ops.reconciliation`'s own tests.

   The records land at the fixture's charge instants, so the rail shows the
   incident where it happened and `charged-not-provisioned` finds them inside
   its window. A customer seeded at wall clock is invisible to the query the
   whole lane exists to run."
  [made-kbs {:keys [kb fixture]}]
  (let [k (first (filter #(= kb (:kb/name %)) made-kbs))
        recon (requiring-resolve 'is.simm.ops.reconciliation/seed-incident!)
        read-fx (requiring-resolve 'is.simm.ops.reconciliation/read-incident)]
    (if-not k
      (log/log! {:level :warn :id ::customers-kb-missing
                 :msg "Scenario seeds customers into a KB that was not created"
                 :data {:kb kb}})
      (let [conn (kbs/connect-kb-database (:kb/db-scope k))
            fx (read-fx (or fixture :incident))
            res (recon conn fx)]
        (log/log! {:level :info :id ::customers-seeded
                   :data {:kb kb :seeded (:seeded res) :window (:window res)}})
        {:customers/kb kb
         :customers/seeded (:seeded res)
         :customers/window (:window res)}))))

(defn load-scenario!
  "Materialize `scenario` (a map, or a keyword naming one under resources/demo)
   into a live workspace owned by `owner-id`. Returns a manifest of what it
   made. Not idempotent: it creates NEW stores each time, so a re-seed is a
   fresh parallel workspace rather than an update."
  [owner-id scenario]
  (let [{:keys [company] :as s} (if (map? scenario)
                                  scenario
                                  (read-scenario scenario))]
    (when-not s
      (throw (ex-info "unknown scenario" {:scenario scenario})))
    (ctx/with-server-context
      ;; order matters: rooms grant against drives and KBs by NAME, so those
      ;; have to exist before a room can be attached to them.
      (let [t0 (earliest-inst s)
            made-kbs (mapv #(load-kb! owner-id t0 %) (:kbs s))
            made-drives (mapv #(load-drive! owner-id %) (:drives s))
            made-rooms (vec (map-indexed (fn [i r] (load-room! owner-id r i))
                                         (:rooms s)))
            ;; last: a proposal branches a seeded KB and is filed against a
            ;; seeded room, so both have to exist
            made-books (vec (keep #(load-book! made-rooms %) (:books s)))
            ;; Customer RECORDS, into the KB the scenario names. Seeded with the
            ;; incident's own instant, not `t0`: these are the six hours the
            ;; webhook was failing, and the reconciliation query windows on
            ;; exactly that. Landing them at seed time would put every account
            ;; outside the window the demo asks about.
            made-customers (vec (keep #(load-customers! made-kbs %) (:customers s)))
            made-proposals (vec (keep #(load-proposal! owner-id made-kbs made-rooms %)
                                      (:proposals s)))]
        (log/log! {:level :info :id ::scenario-loaded
                   :msg "Demo scenario materialized"
                   :data {:company company
                          :narrative-start t0
                          :kbs (mapv :kb/name made-kbs)
                          :pages (reduce + 0 (map :seeded/pages made-kbs))
                          :drives (mapv :drive/name made-drives)
                          :rooms (mapv :room/name made-rooms)
                          :messages (reduce + 0 (map :seeded/messages made-rooms))
                          :proposals (mapv :proposal/title made-proposals)
                          :book-entries (reduce + 0 (map :book/entries made-books))
                          :customers (reduce + 0 (map :customers/seeded made-customers))}})
        {:company company
         :kbs made-kbs
         :drives made-drives
         :rooms made-rooms
         :books made-books
         :customers made-customers
         :proposals made-proposals}))))
