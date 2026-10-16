(ns is.simm.agents.meeting-scribe-test
  "The Meeting scribe persona, against the sandbox it runs in — no model call.

   Two things are pinned. First, every vocabulary call the persona tells the
   agent to make EXISTS in the sandbox it gets (`transcript/*`, `screen/*`,
   `proposal/*`, `kb/*`, `wiki/*`): a persona naming a verb that is not there
   fails at the first turn, silently from the reviewer's side. Second, a
   SCRIPTED run — the clojure_eval programs a model following the persona
   emits, played back — produces what the persona promises: atom and overview
   pages on a review fork (nothing on trunk), one claim per `<h2>` block, a
   verbatim quote block citing the transcript, the page kind, `:S.Page/source`
   and `:S.Page/entry-point`, filed as one proposal."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [sci.core :as sci]
            [dvergr.tools.llm-call :as llm-call]
            [is.simm.agents.room-agents :as ra]
            [is.simm.agents.templates :as templates]
            [is.simm.authority.world :as world]
            [is.simm.model.store :as store]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.proposals :as props]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]
            [is.simm.runtimes.screen-intake :as intake]
            [is.simm.runtimes.transcripts :as transcripts]))

(def ^:private transcript-path "/drive/transcripts/2026-10-16-1400-planning.md")

(def ^:private transcript-text
  "# Planning call\n\n**Ada**: we push the launch to March, after the audit\n**Bob**: agreed, I own the audit checklist\n")

(defn- fresh-kb []
  (let [scope (random-uuid)
        cfg {:store {:backend :memory :id scope}
             :keep-history? true :crypto-hash? true :commit-graph? true
             :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (store/install! conn)
      (branching/register-system! conn scope)
      [scope conn])))

(defn- room-with-scribe! [sys-conn scope]
  (let [room (random-uuid) kb-id (random-uuid) sys (random-uuid) scribe (random-uuid)]
    (d/transact sys-conn [{:party/id scribe :party/type :agent}])
    (d/transact sys-conn
                [{:db/id "s" :system/id sys :system/type :kb :system/name "Wiki"}
                 {:kb/id kb-id :kb/name "Wiki" :kb/db-scope scope :kb/system-id sys
                  :kb/owner scribe :kb/created (java.util.Date.)}
                 {:db/id "r" :room/id room :room/name "Planning" :room/type :group
                  :room/parties [[:party/id scribe]]}
                 {:grant/id (random-uuid) :grant/room "r" :grant/system "s"
                  :grant/permission :read-write :grant/created (java.util.Date.)}])
    {:room room :scribe scribe
     :kb-conns [{:name "Wiki" :conn (branching/get-kb-conn scope) :db-scope scope
                 :kb-id kb-id :room-id room :writable? true}]}))

(defn- transcript-ns
  "The real `transcript/*` vocabulary's KEYS, with the drive reads answered
   from the fixture (the drive needs a live room)."
  [room scribe]
  (let [real (transcripts/sci-namespace room scribe)]
    (assert (= #{'list 'read 'transcribe!} (set (keys real))))
    {'list (fn [] [{:path transcript-path :name "2026-10-16-1400-planning.md" :size 120}])
     'read (fn [p] (when (str/ends-with? (str p) "planning.md") transcript-text))
     'transcribe! (fn [_] (throw (ex-info "not in this test" {})))}))

(defn- scribe-sandbox [kb-conns room scribe]
  (let [sci-ctx (sci/init {})
        overlay-fn #(#'ra/active-overlay room scribe)]
    (#'ra/add-wiki-ns! sci-ctx kb-conns room scribe)
    (#'ra/add-kb-write-ns! sci-ctx kb-conns overlay-fn scribe)
    (#'ra/add-proposal-ns! sci-ctx room scribe)
    (sci/add-namespace! sci-ctx 'screen
                        (merge (intake/sci-namespace room)
                               {'shares (fn [] [{:name "Ada" :from 1000 :until 2000 :active? false}])
                                'history (fn [& _] [{:name "Ada" :at 1500 :blob-id "b1"
                                                     :text "Slide: Launch timeline — March"}])}))
    (sci/add-namespace! sci-ctx 'transcript (transcript-ns room scribe))
    sci-ctx))

(defn- persona-calls
  "Every `(ns/verb` the persona's prompt tells the agent to call."
  []
  (->> (re-seq #"\((transcript|screen|proposal|kb|wiki)/([a-z!?-]+)"
               (:system-prompt templates/meeting-scribe-template))
       (map (fn [[_ n v]] (str n "/" v)))
       set))

(def ^:private scripted-turn
  "What a model following the persona emits, one clojure_eval per step."
  ["(proposal/start! \"Meeting notes: planning call\")"
   "(transcript/list)"
   (str "(transcript/read \"" transcript-path "\")")
   "(screen/shares)"
   "(screen/history {:since 1000 :until 2000 :n 100})"
   "(wiki/search \"launch\")"
   (str "(let [db \"Wiki\" src \"transcripts/2026-10-16-1400-planning.md\""
        "      p (kb/ensure-page! db \"Launch date\")]"
        "  (kb/set-property! db p :S.Page/kind :wiki/atom)"
        "  (kb/set-property! db p :S.Page/source #{src})"
        "  (kb/set-property! db p :S.Page/entry-point src)"
        "  (kb/upsert-block! db p \"<h2>The launch moves to March</h2><p>After the audit.</p>\")"
        "  (kb/upsert-block! db p (str \"<blockquote>\\\"we push the launch to March, after the audit\\\"</blockquote><p>[[\" src \"]]</p>\"))"
        "  (kb/upsert-block! db p (str \"<p><strong>Sources:</strong> [[\" src \"]]</p>\"))"
        "  p)")
   (str "(let [db \"Wiki\" src \"transcripts/2026-10-16-1400-planning.md\""
        "      p (kb/ensure-page! db \"Planning call 2026-10-16\")]"
        "  (kb/set-property! db p :S.Page/kind :wiki/overview)"
        "  (kb/set-property! db p :S.Page/source #{src})"
        "  (kb/set-property! db p :S.Page/entry-point src)"
        "  (kb/upsert-block! db p \"<p>Decided: [[Launch date]]. Open: audit owner's deadline.</p>\")"
        "  (kb/upsert-block! db p (str \"<p><strong>Sources:</strong> [[\" src \"]]</p>\"))"
        "  p)")
   "(proposal/file! \"The planning call moved the launch to March; one atom, one overview.\")"])

(deftest every-call-the-persona-names-exists-in-its-sandbox
  (ctx/with-server-context
    (let [sys-conn (world/fresh-system-conn)]
      (with-redefs [sdb/get-conn (fn [] sys-conn)]
        (let [[scope] (fresh-kb)
              {:keys [room scribe kb-conns]} (room-with-scribe! sys-conn scope)
              sb (scribe-sandbox kb-conns room scribe)
              calls (persona-calls)]
          (is (<= 6 (count calls)) (str "the persona names its verbs: " calls))
          (doseq [c calls]
            (is (fn? (sci/eval-string* sb c)) (str c " is not in the scribe's sandbox"))))))))

(deftest a-scripted-scribe-run-writes-reviewable-atom-and-overview-pages
  (ctx/with-server-context
    (let [sys-conn (world/fresh-system-conn)]
      (with-redefs [sdb/get-conn (fn [] sys-conn)
                    llm-call/cheap-llm-call (fn [& _] {:text "stub summary"})]
        (let [[scope] (fresh-kb)
              {:keys [room scribe kb-conns]} (room-with-scribe! sys-conn scope)
              sb (scribe-sandbox kb-conns room scribe)
              results (mapv #(sci/eval-string* sb %) scripted-turn)
              filed (peek results)
              pid (parse-uuid (str (:proposal filed)))
              p (props/get-proposal pid)
              [fork] (:proposal/forks p)
              branch-db @(branching/get-kb-conn-on-branch
                          scope (keyword (:proposal.fork/branch fork)))
              page (fn [title]
                     (d/pull branch-db '[:S.Page/kind :S.Page/source :S.Page/entry-point
                                         {:block/_parent [:block/content :block/order]}]
                             [:S.Page/title title]))]
          (testing "the transcript and the screens were read"
            (is (str/includes? (nth results 2) "push the launch to March"))
            (is (= "Slide: Launch timeline — March" (:text (first (nth results 4))))))
          (testing "filed as ONE proposal with one fork; nothing on trunk"
            (is (some? pid))
            (is (= 1 (count (:proposal/forks p))))
            (is (nil? (d/q '[:find ?e . :where [?e :S.Page/title "Launch date"]]
                           @(branching/get-kb-conn scope)))))
          (testing "the atom page follows the wiki layout"
            (let [atom (page "Launch date")
                  blocks (map :block/content (sort-by :block/order (:block/_parent atom)))]
              (is (= :wiki/atom (:S.Page/kind atom)))
              (is (= #{"transcripts/2026-10-16-1400-planning.md"} (set (:S.Page/source atom))))
              (is (= "transcripts/2026-10-16-1400-planning.md" (:S.Page/entry-point atom)))
              (is (some #(str/starts-with? % "<h2>") blocks) "a claim is an <h2> block")
              (let [quote (some #(when (str/starts-with? % "<blockquote>") %) blocks)
                    quoted (second (re-find #"<blockquote>\"(.+?)\"</blockquote>" (str quote)))]
                (is (some? quoted))
                (is (str/includes? transcript-text quoted) "the quote is VERBATIM from the transcript")
                (is (str/includes? quote "[[transcripts/2026-10-16-1400-planning.md]]")))
              (is (some #(str/includes? % "<strong>Sources:</strong>") blocks))))
          (testing "one overview links the atom"
            (let [ov (page "Planning call 2026-10-16")]
              (is (= :wiki/overview (:S.Page/kind ov)))
              (is (some #(str/includes? (:block/content %) "[[Launch date]]")
                        (:block/_parent ov))))))))))
