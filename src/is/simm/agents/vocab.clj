(ns is.simm.agents.vocab
  "Give the agent's SCI vocabulary the metadata a Clojure developer expects.

   THE PROBLEM. dvergr already surfaces signatures: `dvergr.sandbox` reads
   `(meta v)` off every injected value and reports `:arglists` and `:doc`, and
   `dev/doc` prints them. But simmis injects its whole vocabulary as bare
   anonymous fns —

     {'ensure-page! (fn [db title] …)}

   — and `(fn …)` carries no metadata, so every one of those contributed
   NOTHING. `dvergr.sandbox` says as much in a comment: \"raw injected fns carry
   no :doc/:arglists\". The agent's only way to learn an arity was to call the
   fn and read the error, which is exactly what we saw: a turn that transacted
   `{:schedule/id nil}` because there was no way to discover the right call.

   THE FIX is small because the machinery already exists — attach the metadata
   and dvergr's introspection lights up. `with-docs` keeps the documentation as
   DATA beside the namespace rather than restructuring the fn maps, so adding a
   verb and documenting it stay one small edit apart and the docs can also be
   rendered outside SCI.

   A missing entry is not an error: undocumented verbs still work, they are just
   invisible to `doc`. That keeps this incremental instead of a flag day."
  (:require [clojure.string :as str]
            [taoensso.telemere :as log]))

(defn with-docs
  "Merge `docs` — `{sym {:arglists [[…]] :doc \"…\"}}` — into `ns-map`'s fns as
   metadata, returning the map ready for `sci/add-namespace!`.

   Warns about documented symbols that no longer exist: a renamed verb whose
   doc entry lingers is worse than no entry, because `doc` would keep
   advertising a call that fails."
  [ns-name ns-map docs]
  (let [orphans (remove (set (keys ns-map)) (keys docs))]
    (when (seq orphans)
      (log/log! {:level :warn :id ::orphan-docs
                 :msg "Documented verbs that no longer exist in the namespace"
                 :data {:ns ns-name :symbols (vec orphans)}}))
    (reduce-kv
     (fn [m sym v]
       (if-let [{:keys [arglists doc]} (get docs sym)]
         (assoc m sym (if (instance? clojure.lang.IObj v)
                        (vary-meta v merge
                                   (cond-> {:name sym}
                                     arglists (assoc :arglists arglists)
                                     doc (assoc :doc doc)))
                        v))
         m))
     ns-map ns-map)))

(defn undocumented
  "Symbols in `ns-map` with no `docs` entry — the checklist for finishing a
   namespace off. Used by the vocab test so coverage can only improve."
  [ns-map docs]
  (vec (sort (remove (set (keys docs)) (keys ns-map)))))

;; ===========================================================================
;; The documentation, as data
;; ===========================================================================
;;
;; `db` throughout is a KB HANDLE, not a datahike db value: the sandbox resolves
;; it through a conn-fn so an active proposal can redirect writes onto a fork
;; branch. Pass what `wiki/pages`-style helpers hand you, or the KB's name.

(def kb-docs
  '{ensure-page!
    {:arglists ([db title])
     :doc "Page uuid for `title`, creating the page (with one empty block) if absent. Idempotent."}

    next-order
    {:arglists ([db page-uuid])
     :doc "Fractional order key that sorts AFTER every existing block on the page. Pass to upsert-block!."}

    upsert-block!
    {:arglists ([db page-uuid content] [db page-uuid content order])
     :doc "Append a block of HTML `content` to a page; returns its uuid. [[Title]] in the text becomes a stored reference, so backlinks work. `order` defaults to the end."}

    archive-page!
    {:arglists ([db title])
     :doc "Hide a page from the browse list without deleting it. Returns its uuid, or nil if no such page."}

    retract-block!
    {:arglists ([db block-uuid])
     :doc "Delete a block outright. Prefer editing when the text is merely wrong — a retraction is what a reviewer sees as a removal."}

    install-attr!
    {:arglists ([db ident codomain] [db ident codomain cardinality])
     :doc "Declare a new attribute by its CODOMAIN object, e.g. (kb/install-attr! db :todo/owner \"S/String\" :one); the value type is derived from it (\"S/String\", \"S/Number\" integer, \"S/Float\", \"S/Boolean\", \"S/Date\", \"S/UUID\", or a type such as \"S/Page\" for a reference), never given. Cardinality :one (default) or :many. Needs a :merge grant of the KB to this room — a :read-write grant writes data under declared attributes, not schema; ask the KB's owner to declare it otherwise. Idempotent. Writing an undeclared attribute REJECTS the whole transaction."}

    upsert-viz-block!
    {:arglists ([db page-uuid viz-spec-edn] [db page-uuid viz-spec-edn caption])
     :doc "Append a block rendering a Vega-Lite spec as a chart."}

    upsert-widget-block!
    {:arglists ([db page-uuid widget-code] [db page-uuid widget-code caption])
     :doc "Append a block whose ClojureScript is evaluated client-side in a curated sandbox."}

    add-type!
    {:arglists ([db entity-uuid type-name])
     :doc "Assign a category-S type to an entity, e.g. \"S/Person\". Types carry properties and an icon."}

    set-property!
    {:arglists ([db entity-uuid attr value])
     :doc "Set one property on an entity. The attribute must exist — see install-attr!."}

    retract-property!
    {:arglists ([db entity-uuid attr])
     :doc "Remove a property from an entity."}

    attributes
    {:arglists ([db])
     :doc "WHAT IS IN THIS DATABASE: every attribute carrying data, with how many entities carry it, domain (S.*) types first. Start here when you need data that is not a wiki page — customers, invoices, anything typed. `wiki/pages` only ever shows pages, so an empty page list does NOT mean an empty database."}

    db
    {:arglists ([db])
     :doc "The database VALUE for a KB name, for use with the ordinary datahike API: (d/q '[:find ?e :where [?e :S.Customer/account-id _]] (kb/db \"Customers\")). Also works with d/pull, d/datoms, d/entity. Immutable snapshot; reads your own uncommitted proposal work when you have a fork open."}

    conn
    {:arglists ([db])
     :doc "The CONNECTION for a KB name, for d/transact. Inside a proposal this resolves to your fork, so transacting through it is governed exactly like kb/upsert-block! — the change lands on your review branch, not on trunk. Prefer the kb/* verbs for pages; use this for typed entities and bulk writes."}

    link
    {:arglists ([db title] [db title display])
     :doc "A cross-database reference to a page BY TITLE, as [[dh://…]] markup to embed in block content. Use this rather than a bare [[Title]] when the target lives in another KB."}

    link-to
    {:arglists ([db entity-uuid] [db entity-uuid display])
     :doc "As `link`, but addressing the entity by uuid — stable across renames."}

    read-page
    {:arglists ([title])
     :doc "This KB's page as {:title :blocks}, or nil. See wiki/read-page to search every attached KB."}})

(def memory-docs
  '{documents {:arglists ([])
               :doc "Source-selected Markdown descriptors from the current file world, with status and selection receipt. Invalid/pending views are explicit."}
    read {:arglists ([path])
          :doc "Exact Markdown source, semantic blocks and properties at a repository-relative path, with selection receipt. Includes file-backed wiki and capture notes; media links/provenance are data, not visual inspection. Missing document is nil; non-ready views carry diagnostics."}
    write! {:arglists ([request])
            :doc "Edit Markdown in the selected worktree with {:path path :expected entry :text source}. Pass the exact :entry from memory/read, or nil for a new file. Refuses concurrent changes. :written acknowledges this worktree edit, not publication; proposal/file! handles review. Never retry an uncertain acknowledgement without reading again."}
    link {:arglists ([path] [path options])
          :doc "Get a scoped Markdown link for an existing shared file, optionally {:label text :block block-uuid}. Missing files are never created. Unpublished proposal/child-world files return :unpublished without a misleading trunk link. File URLs are path-based; explicit block references preserve block identity."}
    search {:arglists ([text])
            :doc "Literal case-sensitive source search, at most 50 hits, with UTF-16 ranges and source/selection identifiers. Receipts borrow source lifetime, not archival retention."}})

(def wiki-docs
  '{read-page
    {:arglists ([title])
     :doc "A page as {:title :blocks} from ANY attached knowledge base, or nil."}

    search
    {:arglists ([query])
     :doc "Ranked full-text search across every attached KB. Returns [{:kb :title :snippet}]."}

    pages
    {:arglists ([])
     :doc "Every page title across the attached KBs, as [{:kb :title}]."}

    backlinks
    {:arglists ([title])
     :doc "Pages whose blocks reference `title` — the inbound half of the wiki graph."}

    neighborhood
    {:arglists ([title])
     :doc "A page with its forward links, backlinks and similar pages in one call. Cheaper and more useful than three separate lookups when orienting."}

    summarize!
    {:arglists ([])
     :doc "Summarize this room's recent conversation into a linked wiki page. Room-bound."}})

(def proposal-docs
  '{start!
    {:arglists ([title])
     :doc "Open a PRIVATE proposal: your subsequent file, memory/*, kb/* and kontor/* writes land on private fork BRANCHES, leaving trunk untouched, until file! or abandon!. Use for anything structural."}

    open-campaign!
    {:arglists ([title])
     :doc "Open a ROOM-WIDE campaign: one shared proposal several agents contribute to. Others join with join-campaign!. EVERY member calls file! when their own part is done, and the LAST one to do so files everyone's work as one proposal, one fork per agent per database. Use when the room is working on one joint change."}

    join-campaign!
    {:arglists ([])
     :doc "Contribute to the room's open campaign: your kb/* and kontor/* writes fork into it instead of into a private proposal. abandon! withdraws just your part."}

    active
    {:arglists ([])
     :doc "What you are contributing to — {:title :campaign? :contributors :forks} — or nil when your writes are going straight to trunk."}

    file!
    {:arglists ([] [rationale])
     :doc "Submit YOUR part for human review. Freezes all current file additions, edits and deletions automatically; git commit is optional. Records the exact commit and its checks so the diff stays inspectable after acceptance. Empty file changes are refused; a failed filing preserves your workspace for retry. On a private proposal this files it immediately. In a campaign it means \"I am done\": the campaign stays open for the other members and files itself when the last of them calls file! too — the reply names who is still outstanding, so tell them in the room. State WHY in `rationale`; the reviewer sees it."}

    release!
    {:arglists ([])
     :doc "Deliberately return to writing straight to trunk after the proposal you were contributing to was filed. Only needed when a governed write was refused because someone else filed the campaign — normally you start a follow-up instead."}

    refresh!
    {:arglists ([])
     :doc "Bring trunk's current state INTO your code fork, so your change is reviewed against the repository as it is now rather than as it was when you started. Call it before file! whenever you have been working a while. If trunk changed files you also changed, they are merged line by line — you only hear about a file where you and trunk edited the SAME lines. For those, open the file, decide what the combined version should say, commit, and refresh! again; everything else is already merged."}

    abandon!
    {:arglists ([])
     :doc "Discard your open work and its branches. In a campaign this withdraws only YOUR forks and leaves the campaign standing for the others. Nothing reaches trunk."}})

(def transcript-docs
  '{list
    {:arglists ([])
     :doc "This room's meeting transcripts — files under /drive/transcripts/ — as [{:path :name :size}], newest first."}

    read
    {:arglists ([path])
     :doc "A transcript's text (Markdown) by its path or file name, or nil. Cite it in a wiki page as [[transcripts/<file>.md]]."}

    transcribe!
    {:arglists ([path])
     :doc "Transcribe an audio file on the drive (\"/drive/call.m4a\") with speech-to-text, save it under /drive/transcripts/ and announce it in the room; returns {:path :message-id}. Slow — minutes for an hour of audio. Only when someone asks."}})
