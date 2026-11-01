;; Private synthetic file stores only. Invoked by native-memory-upgrade-cold-test.
(require '[clojure.java.io :as io] '[clojure.java.shell :as shell] '[clojure.string :as str] '[datahike.api :as d]
         '[dvergr.system.db :as registry] '[dvergr.system.rooms :as systems]
         '[dvergr.rooms :as live] '[dvergr.room.registry :as rooms]
         '[dvergr.discourse :as discourse] '[dvergr.substrate.paths :as paths]
         '[dvergr.chat.schema :as chat] '[geschichte.repo :as repo] '[geschichte.query :as query]
         '[is.simm.model.system-db :as control-model] '[is.simm.runtimes.context :as server]
         '[is.simm.documents.markdown :as markdown] '[is.simm.documents.knowledge-upgrade :as foundation]
         '[is.simm.ops.offline-memory-upgrade :as upgrade] '[is.simm.ops.offline-memory-upgrade-cli :as cli]
         '[org.replikativ.spindel.engine.core :as ec] '[org.replikativ.spindel.engine.context :as contexts])
(defn fixture-git! [directory & args]
  (let [environment (into {} (remove #(str/starts-with? (key %) "GIT_")) (System/getenv))
        environment (assoc environment "GIT_CONFIG_NOSYSTEM" "1" "GIT_CONFIG_GLOBAL" "/dev/null")
        result (apply shell/sh (concat ["git" "-c" "core.hooksPath=/dev/null" "-c" "user.name=Fixture"
                                        "-c" "user.email=fixture@localhost" "-C" (str directory)] args [:env environment]))]
    (assert (zero? (:exit result)) "Local Git fixture command failed")))
(try
  (let [home (first *command-line-args*)
        source (io/file home "source") _ (.mkdirs source)
        _ (fixture-git! source "init" "--initial-branch=main")
        _ (spit (io/file source "README.md") "# Preserved legacy knowledge\n")
        _ (fixture-git! source "add" ".")
        _ (fixture-git! source "commit" "-m" "Original local source")
        cfg {:store {:backend :file :path (str home "/.dvergr/system-db") :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :attribute-refs? false
             :writer {:backend :self :writer-ownership :shared}}
        _ (d/create-database cfg) control (d/connect cfg)
        actor (random-uuid) root (contexts/create-execution-context) previous-home (paths/home)
        slug (str "legacy-cold-" (random-uuid)) result (atom nil)]
    (try
      (paths/set-home! home)
      (d/transact control (vec (concat registry/schema registry/actor-schema registry/assignment-schema registry/pricing-schema registry/task-schema)))
      (chat/ensure-full-schema! control) (d/transact control control-model/schema)
      (d/transact control [{:party/id actor :party/type :human :actor/kind :human}])
      (with-redefs [registry/get-conn (constantly control) control-model/get-conn (constantly control)
                    server/server-context root]
        (binding [ec/*execution-context* root]
          (let [native-id (live/create-room! {:title "Original legacy Room" :slug slug :type :personal-ai :parent-id false
                                              :repo-source (str source) :repo-branch "main"})
                room (rooms/lookup native-id) id (:room/id (registry/room-by-slug slug))
                _ (d/transact control [{:room/id id :room/type :personal-ai :room/created-by actor
                                        :room/owner [:party/id actor] :room/parties [[:party/id actor]]
                                        :room/content-db-scope (random-uuid)}])
                workspace (systems/default-workspace @control id) repository (:conn workspace)
                system (:room/default-repository (d/entity @control [:room/id id]))]
            (assert room)
            (repo/write! repository "staged.md" (markdown/source-bytes "# Staged original\n"))
            (repo/stage-all! repository)
            (repo/write! repository "unstaged.md" (markdown/source-bytes "# Unstaged original\n"))
            (reset! result {:home home :control cfg :repository (:config @repository)
                            :request {:id (random-uuid) :actor actor :room id :at (java.util.Date.)
                                      :target {:system (:system/id system) :scope (:system/scope system)
                                               :repository (:geschichte.repo/id (query/repository @repository))
                                               :store (get-in @repository [:config :store :id])}}
                            :source (foundation/source-id @repository) :head (repo/head-commit repository)})
            (swap! result assoc
                   :room-before (d/pull @control '[:room/id :room/created-by :room/type :room/content-db-scope
                                                   :room/provisioning-state :room/provisioning-version
                                                   {:room/owner [:party/id]}] [:room/id id])
                   :grants-before (vec (sort-by :db/id (map #(d/pull @control '[*] %)
                                                            (d/q '[:find [?g ...] :in $ ?r :where [?room :room/id ?r] [?g :grant/room ?room]] @control id)))))
            (assert (nil? (get-in @result [:room-before :room/provisioning-state])))
            (assert (nil? (get-in @result [:room-before :room/provisioning-version])))
            (discourse/close-room! room {:close-context? false})
            (systems/release-room-stores!))))
      (contexts/close-context! root)
      (let [repository (d/connect (:repository @result))]
        (try
          (assert (= :published (:status (upgrade/upgrade! control repository (upgrade/inspect! control repository (:request @result))))))
          (assert (= (:source @result) (foundation/source-id @repository)))
          (finally (cli/release! repository))))
      (spit (io/file home "fixture.edn") (pr-str @result))
      (println :native-legacy-created-retired-and-upgraded home)
      (finally (paths/set-home! previous-home) (cli/release! control))))

  (shutdown-agents)
  (System/exit 0)
  (catch Throwable error
    (.printStackTrace error)
    (shutdown-agents)
    (System/exit 1)))
