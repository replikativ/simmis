;; Fresh-process native hydration, not just a Datahike reconnect.
(require '[clojure.edn :as edn] '[clojure.java.io :as io] '[dvergr.substrate.paths :as paths])
(def home (first *command-line-args*))
(def mode (second *command-line-args*))
(assert (and home (#{"edit" "check"} mode)))
(paths/set-home! home)
(require '[datahike.api :as d] '[dvergr.system.db :as registry] '[dvergr.system.rooms :as systems]
         '[dvergr.rooms :as native] '[dvergr.discourse :as discourse]
         '[is.simm.model.system-db :as control-model] '[is.simm.runtimes.context :as server]
         '[is.simm.agents.room-agents :as agents] '[is.simm.documents.profile :as profile]
         '[is.simm.documents.starter-bundle :as starter] '[is.simm.documents.knowledge-upgrade :as foundation]
         '[is.simm.documents.markdown :as markdown] '[is.simm.documents.view :as view]
         '[is.simm.model.personal-rooms :as personal] '[is.simm.ops.offline-memory-upgrade-cli :as cli]
         '[geschichte.repo :as repo] '[org.replikativ.spindel.engine.core :as ec]
         '[org.replikativ.spindel.engine.context :as contexts])
(try
  (let [fixture (edn/read-string (slurp (io/file home "fixture.edn")))
        control (d/connect (:control fixture)) root server/server-context
        request (:request fixture) id (:room request)]
    (try
      (with-redefs [registry/get-conn (constantly control) control-model/get-conn (constantly control)]
        (binding [ec/*execution-context* root]
          (systems/hydrate-rooms! {:repository-profiles (profile/policies (starter/read-packaged!))})
          (native/hydrate-registry! root (profile/registration-options))
          (let [room (agents/live-room id) repository (:conn (systems/default-workspace @control id))
                row (d/pull @control '[:room/id :room/created-by :room/type :room/content-db-scope
                                       :room/provisioning-state :room/provisioning-version
                                       {:room/owner [:party/id]}] [:room/id id])
                grants (vec (sort-by :db/id (map #(d/pull @control '[*] %)
                                                 (d/q '[:find [?g ...] :in $ ?r :where [?room :room/id ?r] [?g :grant/room ?room]] @control id))))]
            (assert room "Actual cold Simmis live-room lookup must register")
            (assert (identical? room (agents/live-room id)))
            (assert (= row (:room-before fixture)))
            (assert (= grants (:grants-before fixture)))
            (assert (= id (#'personal/existing-id @control (:actor request))))
            (if (= "edit" mode)
              (do
                (assert (= (:source fixture) (foundation/source-id @repository)))
                (assert (= (:head fixture) (repo/head-commit repository)))
                (repo/write! repository "README.md" (markdown/source-bytes "---\nsimmis-id: needs-raw-repair\n---\n# A legitimate later edit\n"))
                (repo/stage-all! repository)
                (repo/commit! repository {:message "Post-upgrade edit remains repairable"}))
              (do
                (assert (not= (:source fixture) (foundation/source-id @repository)))
                (assert (not= :ready (:status (view/prepare @repository profile/selection-options))))
                (assert (true? (profile/legacy-room-ready? @control id @repository)))))
            (discourse/close-room! room {:close-context? false})
            (systems/release-room-stores!))))
      (contexts/close-context! root)
      (println :full-cold-native-verified mode)
      (finally (cli/release! control))))
  (shutdown-agents)
  (System/exit 0)
  (catch Throwable error
    (binding [*out* *err*] (.printStackTrace error))
    (shutdown-agents)
    (System/exit 1)))
