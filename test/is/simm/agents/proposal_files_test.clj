(ns is.simm.agents.proposal-files-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.async :as async]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as store]
            [dvergr.substrate.geschichte :as geschichte]
            [dvergr.tools.llm-call :as llm]
            [geschichte.repo :as repo]
            [is.simm.agents.room-agents :as agents]
            [is.simm.authority.world :as authority]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.proposal-publication :as publication]
            [is.simm.ops.proposals :as proposals]
            [is.simm.ops.proposal-evidence :as evidence]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.context :as runtime]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.ops.semantic-diff :as diff]
            [is.simm.runtimes.branching :as branching]
            [org.replikativ.spindel.engine.core :as ec]
            [yggdrasil.protocols :as systems]
            [muschel.fs :as fs]
            [sci.core :as sci]))

(defn with-repo [f]
  (authority/with-world [control w]
    (let [room (discourse/make-room {:id (keyword (str "proposal-files-" (random-uuid)))
                                     :store (store/make)})]
      (try
        (workspace/ensure-workspace! room)
        (binding [ec/*execution-context* (:ctx room)]
          (let [native (geschichte/current-workspace) sys (:system native)
                conn (:conn native) scope (:repo-sys w) sid (systems/system-id sys)]
            (repo/write! conn "base.txt" (.getBytes "before" "UTF-8"))
            (repo/stage-all! conn) (repo/commit! conn {:message "Base"})
            (with-redefs-fn {#'branching/get-repo-system (constantly sys)
                             #'branching/repo-scope-path (constantly "fixture")
                             #'branching/repo-system-id (constantly sid)
                             #'branching/room-ctx-owning-repo (constantly (:ctx room))
                             #'agents/room-repo-scope (constantly scope)
                             #'publication/publish! (constantly nil)
                             #'llm/cheap-llm-call (fn [& _] {:text "Website change"})}
              #(f {:conn conn :scope scope :room (:room w) :agent (:team-agent w) :owner (:owner w) :stranger (:non-member w)
                   :native-room room :control control}))))
        (finally (discourse/close-room! room))))))

(deftest dirty-files-freeze-and-survive-branch-removal-and-gc
  (with-repo
    (fn [{:keys [conn scope]}]
      (let [base (str (:geschichte.commit/id (repo/head-commit conn)))
            branch :website-review
            _ (branching/branch-repo! scope "unused")
            sys (branching/get-repo-system scope)
            _ (systems/branch! sys branch :main)
            fork (branching/open-repo-fork! scope branch)]
        (try
          (let [private (branching/repo-fork-conn fork)]
            (repo/write! private "app/index.html" (.getBytes "<h1>Published</h1>" "UTF-8"))
            (repo/remove! private "base.txt")
            (let [head (branching/freeze-repo-fork! fork "Freeze website")]
              (is (nil? (repo/read conn "app/index.html")))
              (is (= head (branching/publish-repo-fork! fork branch)))
              (branching/retain-repo-head! scope head)
              (is (= #{"app/index.html" "base.txt"}
                     (set (map :path (:files (diff/semantic-repo-diff scope branch :base-commit base :head-commit head))))))
              (branching/discard-repo-branch! scope branch)
              ;; Published physical ancestry is retained under Simmis's
              ;; orphan-only GC policy, independently of logical branch removal.
              (branching/close-repo-fork! fork)
              (async/<!! (d/gc-storage conn))
              (is (= "<h1>Published</h1>" (String. ^bytes (repo/read-at conn (parse-uuid head) "app/index.html") "UTF-8")))
              (is (= "before" (String. ^bytes (repo/read-at conn (parse-uuid base) "base.txt") "UTF-8")))
              (is (= 2 (get-in (diff/semantic-repo-diff scope branch :base-commit base :head-commit head)
                               [:counts :files-changed])))))
          (finally (when (contains? (systems/branches sys) branch) (branching/discard-repo-branch! scope branch))))))))

(deftest publication-failure-keeps-the-private-owner-and-frozen-content
  (with-repo
    (fn [{:keys [scope]}]
      (let [sys (branching/get-repo-system scope) branch :retry-review
            _ (systems/branch! sys branch :main)
            fork (branching/open-repo-fork! scope branch)
            key [scope (random-uuid)]
            overlay (atom {:title "Retry" :ctx-forks {key fork} :forks {key branch}})]
        (try
          (repo/write! (branching/repo-fork-conn fork) "app/index.html" (.getBytes "Keep this" "UTF-8"))
          (with-redefs [branching/publish-repo-fork! (fn [& _] (throw (ex-info "No ACK" {})))]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No ACK" (#'agents/publish-code-forks! overlay))))
          (is (identical? fork (get-in @overlay [:ctx-forks key])))
          (is (= "Keep this" (String. ^bytes (repo/read (branching/repo-fork-conn fork) "app/index.html") "UTF-8")))
          (#'agents/publish-code-forks! overlay)
          (is (= (get-in @overlay [:heads key]) (branching/repo-head-id scope branch)))
          (finally (branching/close-repo-fork! fork) (branching/discard-repo-branch! scope branch)))))))

(deftest filing-freezes-writes-checks-that-version-and-keeps-evidence-after-acceptance
  (with-repo
    (fn [{:keys [conn scope room agent]}]
      (let [interpreter (sci/init {}) overlay (#'agents/private-overlay room agent)]
        (try
          (#'agents/add-proposal-ns! interpreter room agent)
          (sci/eval-string* interpreter "(proposal/start! \"Website\")")
          (let [fork (#'agents/repo-fork! overlay room agent)
                private (branching/repo-fork-conn fork)]
            (repo/write! private "app/index.html" (.getBytes "<h1>Reviewed website</h1>" "UTF-8"))
            (repo/write! private "site_test.clj"
                         (.getBytes "(ns site-test (:require [clojure.test :refer [deftest is]])) (deftest present (is (.contains (slurp \"app/index.html\") \"Reviewed website\")))" "UTF-8"))
            (let [result (sci/eval-string* interpreter "(proposal/file! \"Publish the website\")")
                  id (parse-uuid (:proposal result)) p (proposals/get-proposal id)
                  contribution (first (:proposal/forks p)) head (:proposal.fork/head-commit contribution)]
              (is (:closed @overlay))
              (is (not= (:proposal.fork/base-commit contribution) head))
              (is (= :pass (get-in contribution [:proposal.fork/checks :proposal.check/status]))
                  (get-in contribution [:proposal.fork/checks :proposal.check/output]))
              (is (= head (get-in contribution [:proposal.fork/checks :proposal.check/head])))
              (is (nil? (repo/read conn "app/index.html"))))
            ;; Keep decisions outside the native room fixture's root context.
            (let [p (first (proposals/list-proposals :status :open)) id (:proposal/id p)]
              (proposals/accept-proposal! id)
              (let [accepted (proposals/get-proposal id) f (first (:proposal/forks accepted))]
                (is (= :accepted (:proposal/status accepted)))
                (is (= "<h1>Reviewed website</h1>" (String. ^bytes (repo/read conn "app/index.html") "UTF-8")))
                (is (= 2 (get-in (first (diff/proposal-diff accepted)) [:counts :files-changed])))
                (is (= 1 (get-in f [:proposal.fork/checks :proposal.check/tests]))))))
          (finally (swap! @#'agents/proposal-overlays dissoc [room agent])))))))

(defn- published-contribution! [scope branch path text]
  (let [sys (branching/get-repo-system scope) base (branching/repo-head-id scope :main)]
    (systems/branch! sys branch :main)
    (let [fork (branching/open-repo-fork! scope branch)]
      (try
        (repo/write! (branching/repo-fork-conn fork) path (.getBytes text "UTF-8"))
        (let [head (branching/freeze-repo-fork! fork "Review")]
          (branching/publish-repo-fork! fork branch)
          (branching/retain-repo-head! scope head)
          {:scope scope :branch branch :system-type :repo :base-commit base :head-commit head})
        (finally (branching/close-repo-fork! fork))))))

(deftest immutable-evidence-is-part-of-filing-identity
  (with-repo
    (fn [{:keys [scope room agent]}]
      (let [f (published-contribution! scope :identity "app/index.html" "One")
            filing {:id (random-uuid) :title "Site" :room room :author agent :forks [f]}]
        (proposals/file-proposal! filing)
        (is (thrown? clojure.lang.ExceptionInfo
                     (proposals/file-proposal! (assoc-in filing [:forks 0 :head-commit] (str (random-uuid))))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (proposals/file-proposal! (assoc-in filing [:forks 0 :checks]
                                                         {:status :fail :head (:head-commit f)}))))))))

(deftest accepted-siblings-do-not-block-remaining-contributions
  (with-repo
    (fn [{:keys [scope room agent]}]
      (let [a (published-contribution! scope :first-review "first.txt" "One")
            b (published-contribution! scope :second-review "second.txt" "Two")
            id (proposals/file-proposal! {:title "Two contributions" :room room :author agent :forks [a b]})]
        (proposals/accept-fork! id scope :first-review)
        (is (not (contains? (systems/branches (branching/get-repo-system scope)) :first-review)))
        (proposals/accept-proposal! id)
        (is (= :accepted (:proposal/status (proposals/get-proposal id))))))))

(deftest landing-never-follows-a-branch-that-advances-after-preflight
  (with-repo
    (fn [{:keys [conn scope room agent]}]
      (let [branch :moving-review f (published-contribution! scope branch "app/index.html" "Reviewed")
            id (proposals/file-proposal! {:title "Frozen" :room room :author agent :forks [f]})
            land branching/land-frozen-repo! newer (atom nil)]
        (with-redefs [branching/land-frozen-repo!
                      (fn [s b h]
                        (let [fork (branching/open-repo-fork! scope branch)]
                          (try
                            (repo/write! (branching/repo-fork-conn fork) "app/index.html" (.getBytes "UNREVIEWED" "UTF-8"))
                            (reset! newer (branching/freeze-repo-fork! fork "New private work"))
                            (branching/publish-repo-fork! fork branch)
                            (finally (branching/close-repo-fork! fork))))
                        (land s b h))]
          (proposals/accept-proposal! id))
        (is (= "Reviewed" (String. ^bytes (repo/read conn "app/index.html") "UTF-8")))
        (is (= @newer (branching/repo-head-id scope branch)) "Concurrent work remains owned by its branch")))))

(deftest lost-filing-ack-can-be-retried-after-human-acceptance
  (with-repo
    (fn [{:keys [conn scope room agent]}]
      (let [interpreter (sci/init {}) overlay (#'agents/private-overlay room agent)
            file! proposals/file-proposal!]
        (try
          (#'agents/add-proposal-ns! interpreter room agent)
          (sci/eval-string* interpreter "(proposal/start! \"ACK retry\")")
          (let [fork (#'agents/repo-fork! overlay room agent)]
            (repo/write! (branching/repo-fork-conn fork) "app/index.html" (.getBytes "Recoverable" "UTF-8"))
            (with-redefs [proposals/file-proposal! (fn [filing] (file! filing) (throw (ex-info "Lost ACK" {})))]
              (is (thrown? Exception (sci/eval-string* interpreter "(proposal/file!)"))))
            (let [id (get-in @overlay [:filing-value :id])]
              (is (uuid? id))
              (is (seq (:ctx-forks @overlay)) "The failed handoff retains its cleanup owner")
              (is (false? (:abandoned (sci/eval-string* interpreter "(proposal/abandon!)"))))
              (proposals/accept-proposal! id)
              (with-redefs [llm/cheap-llm-call (fn [& _] (throw (ex-info "No repeat summary" {})))]
                (is (= (str id) (:proposal (sci/eval-string* interpreter "(proposal/file!)")))))
              (is (:closed @overlay))
              (is (= :accepted (:proposal/status (proposals/get-proposal id))))))
          (finally (swap! @#'agents/proposal-overlays dissoc [room agent])))))))

(deftest no-op-filing-refuses-without-dropping-the-workspace
  (with-repo
    (fn [{:keys [room agent]}]
      (let [interpreter (sci/init {}) overlay (#'agents/private-overlay room agent)]
        (try
          (#'agents/add-proposal-ns! interpreter room agent)
          (sci/eval-string* interpreter "(proposal/start! \"No changes\")")
          (let [fork (#'agents/repo-fork! overlay room agent) before (count (proposals/list-proposals))]
            (is (thrown-with-msg? Exception #"No file changes" (sci/eval-string* interpreter "(proposal/file!)")))
            (is (= before (count (proposals/list-proposals))))
            (is (seq (:ctx-forks @overlay)))
            (is (not (:closed @overlay)))
            (branching/close-repo-fork! fork))
          (finally (swap! @#'agents/proposal-overlays dissoc [room agent])))))))

(deftest historical-audit-is-scoped-read-only-and-owns-the-actual-room
  (with-repo
    (fn [{:keys [conn scope room agent owner stranger native-room control]}]
      (let [f (published-contribution! scope :history "app/index.html" "History")
            id (proposals/file-proposal! {:title "Keep evidence" :room room :author agent :forks [f]})
            host {:execution-ctx (:ctx native-room)
                  ::reads/owner {:control-conn control :execution-ctx (:ctx native-room)}}
            semantic diff/semantic-repo-diff]
        (proposals/accept-proposal! id)
        (with-redefs [runtime/server-context (:ctx native-room)
                      agents/registered-room (fn [id] (when (= id room) native-room))
                      admission/default-binding (fn [& _] {:registry-system scope :conn conn
                                                           :system (branching/get-repo-system scope)})
                      diff/semantic-repo-diff (fn [& args]
                                                (is (seq (get @(:meta native-room) preparation/leases-key)))
                                                (apply semantic args))]
          (is (nil? (evidence/audit host stranger id)))
          (let [receipt (evidence/audit host owner id)]
            (is (= :accepted (:status receipt)))
            (is (= "app/index.html" (get-in receipt [:forks 0 :files 0 :path]))))
          (is (empty? (get @(:meta native-room) preparation/leases-key))))))))

(deftest native-threaded-eval-can-file-without-holding-its-callers-monitor
  (with-repo
    (fn [{:keys [room agent native-room]}]
      (let [overlay (#'agents/private-overlay room agent)
            cctx (dvergr.chat.context/create-chat-context
                  {:execution-context (:ctx native-room)})
            interpreter (dvergr.chat.context/sci-context-in cctx (:ctx native-room))
            tool (#'agents/wrap-repo-tool (dvergr.tools/get-tool "clojure_eval")
                                          (constantly overlay) room agent)
            write (#'agents/wrap-repo-tool (dvergr.tools/get-tool "write_file")
                                           (constantly overlay) room agent)
            tctx (dvergr.tools/make-context {:sci-ctx interpreter :chat-ctx cctx
                                             :execution-ctx (:ctx native-room)})]
        (try
          (#'agents/add-proposal-ns! interpreter room agent)
          (sci/eval-string* interpreter "(proposal/start! \"Native dispatch\")")
          (is (= :success (:type ((:execute write) {:path "app/index.html" :content "Native site"} tctx))))
          (let [job (future ((:execute tool) {:code "(proposal/file! \"Native worker files\")"} tctx))
                result (deref job 15000 ::timed-out)]
            (is (not= ::timed-out result) "Native process dispatch must not deadlock on caller's overlay lock")
            (is (= :success (:type result)) (pr-str result))
            (is (:closed @overlay)))
          (finally
            (dvergr.chat.context/close-chat! cctx)
            (swap! @#'agents/proposal-overlays dissoc [room agent])))))))

(deftest open-stream-cannot-publish-after-filing-or-workspace-replacement
  (with-repo
    (fn [{:keys [room agent]}]
      (let [interpreter (sci/init {}) overlay (#'agents/private-overlay room agent)
            captured (atom nil)
            capture (#'agents/wrap-repo-tool {:name "write_file"
                                              :execute (fn [_ ctx] (reset! captured ctx))}
                                             (constantly overlay) room agent)]
        (try
          (#'agents/add-proposal-ns! interpreter room agent)
          (sci/eval-string* interpreter "(proposal/start! \"Stream guard\")")
          ((:execute capture) {} {})
          (let [filesystem (:filesystem @captured)
                stream (fs/-open-sink filesystem "/late.txt" false)]
            (.write stream (.getBytes "Late" "UTF-8"))
            (fs/write-string! filesystem "/ready.txt" "Ready" false)
            (sci/eval-string* interpreter "(proposal/file! \"Ready file only\")")
            (is (thrown? clojure.lang.ExceptionInfo (.close stream)))
            (is (thrown? clojure.lang.ExceptionInfo (fs/-touch filesystem "/late.txt"))))
          (finally (swap! @#'agents/proposal-overlays dissoc [room agent])))))))

(deftest refused-stream-writes-never-publish-a-buffered-prefix
  (with-repo
    (fn [{:keys [room agent]}]
      (let [interpreter (sci/init {}) overlay (#'agents/private-overlay room agent)
            captured (atom nil)
            capture (#'agents/wrap-repo-tool {:name "write_file" :execute (fn [_ ctx] (reset! captured ctx))}
                                             (constantly overlay) room agent)]
        (try
          (#'agents/add-proposal-ns! interpreter room agent)
          (sci/eval-string* interpreter "(proposal/start! \"Oversize guard\")")
          ((:execute capture) {} {})
          (let [filesystem (:filesystem @captured)]
            (fs/write-string! filesystem "/existing.txt" "Keep original" false)
            (doseq [path ["/existing.txt" "/new.txt"]
                    invalid? [false true]]
              (is (thrown? Exception
                           (with-open [out (fs/-open-sink filesystem path false)]
                             (.write out (.getBytes "Partial" "UTF-8"))
                             (if invalid?
                               (.write out (byte-array 1) 2 1)
                               (.write out (byte-array (* 16 1024 1024))))))))
            (is (= "Keep original" (fs/-read-file filesystem "/existing.txt")))
            (is (nil? (fs/-read-file filesystem "/new.txt"))))
          (finally
            (sci/eval-string* interpreter "(proposal/abandon!)")
            (swap! @#'agents/proposal-overlays dissoc [room agent])))))))

(deftest closed-tool-context-cannot-use-a-direct-expected-content-write
  (with-repo
    (fn [{:keys [conn room agent]}]
      (let [interpreter (sci/init {}) overlay (#'agents/private-overlay room agent)
            captured (atom nil)
            capture (#'agents/wrap-repo-tool {:name "write_file" :execute (fn [_ ctx] (reset! captured ctx))}
                                             (constantly overlay) room agent)]
        (try
          (#'agents/add-proposal-ns! interpreter room agent)
          (sci/eval-string* interpreter "(proposal/start! \"Closed native write\")")
          ((:execute capture) {} {})
          (fs/write-string! (:filesystem @captured) "/ready.txt" "Ready" false)
          (sci/eval-string* interpreter "(proposal/file! \"Ready\")")
          ((:execute capture) {} {})
          (is (nil? (:workspace @captured)))
          (is (thrown? clojure.lang.ExceptionInfo
                       (dvergr.tools/workspace-write! @captured "/unexpected.txt" "Must refuse" {:expect nil})))
          (is (nil? (repo/read conn "unexpected.txt")))
          (finally (swap! @#'agents/proposal-overlays dissoc [room agent])))))))

(deftest registered-live-sci-can-write-memory-and-file-in-one-evaluation
  (with-repo
    (fn [{:keys [conn room agent native-room]}]
      (let [cctx (assoc (dvergr.chat.context/create-chat-context
                         {:execution-context (:ctx native-room)}) :capability-id (random-uuid))
            interpreter (dvergr.chat.context/sci-context-in cctx (:ctx native-room))
            actor (keyword "party" (str agent))
            overlay (#'agents/private-overlay room agent)
            tool (#'agents/wrap-repo-tool (dvergr.tools/get-tool "clojure_eval")
                                          (constantly overlay) room agent)
            tctx (dvergr.tools/make-context {:sci-ctx interpreter :chat-ctx cctx
                                             :execution-ctx (:ctx native-room)})
            open! branching/open-repo-fork! selections (atom [])]
        (try
          (dvergr.runtime.ctx/install-sandbox-binding! (:ctx native-room) (:capability-id cctx)
                                                       {:agent-id actor :room-runtime-id (:id native-room)})
          (#'agents/add-proposal-ns! interpreter room agent)
          (#'agents/add-source-memory-ns! interpreter cctx native-room room agent)
          (sci/eval-string* interpreter "(def preserved (atom :unchanged))")
          (with-redefs [branching/open-repo-fork!
                        (fn [& args]
                          (let [fork (apply open! args)]
                            (swap! selections conj
                                   (binding [ec/*execution-context* (:child-ctx fork)]
                                     {:systems (set (keys (org.replikativ.spindel.yggdrasil/registered-systems)))
                                      :components (ec/get-state [:world/forkable-components])}))
                            fork))]
            (let [result ((:execute tool)
                          {:code "(do (proposal/start! \"Inline memory\") (memory/write! {:path \"wiki/Note.md\" :expected nil :text \"# Test\"}) (proposal/file! \"Memory write\"))"} tctx)
                  id (some-> result :metadata :value :proposal parse-uuid)
                  proposal (when id (proposals/get-proposal id))]
              (is (= :success (:type result)) (pr-str result))
              (is (= :open (:proposal/status proposal)))
              (is (not= (get-in proposal [:proposal/forks 0 :proposal.fork/base-commit])
                        (get-in proposal [:proposal/forks 0 :proposal.fork/head-commit])))
              (is (nil? (repo/read conn "wiki/Note.md")))))
          (is (= 2 (count @selections)) "Private workspace and independent frozen check")
          (doseq [selection @selections]
            (is (= #{(systems/system-id (branching/get-repo-system nil))} (:systems selection)))
            (is (empty? (:components selection))))
          (is (= :unchanged (sci/eval-string* interpreter "@preserved")))
          (finally
            (dvergr.chat.context/close-chat! cctx)
            (swap! @#'agents/proposal-overlays dissoc [room agent])))))))
