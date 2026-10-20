(ns is.simm.ops.owned-workflow-authoring-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.rooms.files :as native]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.model.access :as access]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as owned]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as source]))

(defn- seed! [conn files]
  (doseq [[path text] files]
    (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8")))
  (repo/stage-all! conn)
  (repo/commit! conn {:message "Seed authoring source"}))

(defn- principal [w] (ops/party-principal (:member w)))
(defn- opened [host w]
  (:data (owned/details! host (principal w) (str (:room w)) {:name "echo"})))
(defn- request [opened edits] {:name "echo" :expected-revision (:revision opened) :files edits})
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))

(deftest real-owned-write-preserves-other-selection-modes-and-original-ack-source
  (doseq [order [true false]]
    (@#'fixture/with-fixture order
                             (fn [{:keys [conn w host owned attached attached-descriptor]}]
                               (seed! owned source/fixture)
                               (repo/write! owned "workflows/echo/gold.edn" (.getBytes "{:original true}" "UTF-8") {:mode 33261})
                               (repo/stage-all! owned) (repo/commit! owned {:message "Executable fixture"})
                               (seed! attached (assoc source/fixture "gold.edn" "OTHER"))
                               (let [before (opened host w) selected (workspace/selected-system-id)
                                     original-stamp (:target-stamp (reads/resolve-host-read! host (principal w) (str (:room w))))
                                     other {:head (query/refs @attached) :tree (query/worktree @attached)}
                                     native-write native/write-to-completion! calls (atom 0)
                                     text "\uFEFF{:answer \"资料\"}\r\n"
                                     answer (with-redefs [native/write-to-completion!
                                                          (fn [& args]
                                                            (swap! calls inc)
                                                            (let [result (apply native-write args)]
                                     ;; A later raw edit must not rewrite the ACK response.
                                                              (repo/write! owned "workflows/echo/gold.edn" (.getBytes "Later draft" "UTF-8"))
                                                              (d/transact conn [{:room/id (:room w) :room/default-repository
                                                                                 [:system/id (:registry-system attached-descriptor)]}])
                                                              result))]
                                              (authoring/write! host (principal w) (str (:room w))
                                                                (update (request before {"gold.edn" text "new.md" "New λ"})
                                                                        :expected-revision str/upper-case)))]
                                 (is (= 1 @calls))
                                 (is (true? (get-in answer [:write :applied?])))
                                 (is (= original-stamp (:target-stamp answer)))
                                 (is (= text (get-in answer [:source :files "gold.edn"])))
                                 (is (= "New λ" (get-in answer [:source :files "new.md"])))
                                 (is (= 33261 (get-in answer [:write :subtree :entries "gold.edn" :mode])))
                                 (is (= 33188 (get-in answer [:write :subtree :entries "new.md" :mode])))
                                 (is (= (get-in answer [:source :revision]) (get-in answer [:source :inspection :revision])))
                                 (is (= (get source/fixture "checker.clj") (get-in answer [:source :files "checker.clj"])))
                                 (is (= (get (query/worktree @owned) "workflows/echo/checker.clj")
                                        (get-in answer [:write :subtree :entries "checker.clj"])))
                                 (is (not= (:revision before) (get-in answer [:source :revision])))
                                 (is (= "Later draft" (String. ^bytes (repo/read owned "workflows/echo/gold.edn") "UTF-8")))
                                 (is (= selected (workspace/selected-system-id)))
                                 (is (= other {:head (query/refs @attached) :tree (query/worktree @attached)})))))))

(deftest stale-owned-source-compares-normal-metadata-before-bodies-or-native-dispatch
  (@#'fixture/with-fixture true
                           (fn [{:keys [conn w host owned attached attached-descriptor]}]
                             (seed! owned source/fixture) (seed! attached source/fixture)
                             (let [before (opened host w)]
                               (d/transact conn [{:room/id (:room w) :room/default-repository
                                                  [:system/id (:registry-system attached-descriptor)]}])
                               (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Stale read loaded body")))
                                             native/write-to-completion! (fn [& _] (throw (AssertionError. "Stale write dispatched")))]
                                 (is (= ::authoring/conflict
                                        (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w))
                                                                                    (request before {"gold.edn" "Edit"}))))))))))))

(deftest closed-dto-strict-unicode-and-denied-writers-do-not-hydrate
  (@#'fixture/with-fixture false
                           (fn [{:keys [w host hydrations]}]
                             (let [base {:name "echo" :expected-revision (str (random-uuid)) :files {"gold.edn" "Text"}}
                                   surrogate (String. (char-array [(char 0xd800)]))]
                               (doseq [invalid [(dissoc base :expected-revision) (assoc base :mode 33261)
                                                (assoc base :expected-revision "1-1-1-1-1")
                                                (assoc base :files {"gold.edn" surrogate})
                                                (assoc base :files {(str surrogate ".md") "Text"})
                                                (assoc base :files {"a//b.md" "A" "a/b.md" "B"})]]
                                 (is (= ::authoring/invalid (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w)) invalid)))))))
                               (doseq [path ["/absolute.md" "../escape.md" ".git/config" "a\\b.md" "a\tb.md" "a\nb.md"]]
                                 (is (= ::authoring/invalid
                                        (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w))
                                                                                    (assoc base :files {path "X"}))))))))
                               (with-redefs [workflows/max-file-bytes 1]
                                 (is (= ::authoring/invalid
                                        (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w))
                                                                                    (assoc base :files {"gold.edn" "λ"}))))))))
                               (with-redefs [workflows/max-bundle-bytes 1]
                                 (is (= ::authoring/invalid
                                        (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w))
                                                                                    (assoc base :files {"a.md" "A" "b.md" "B"}))))))))
                               (with-redefs [workflows/normalized-edits (fn [& _] (throw (ex-info "SECRET/helper" {:type ::authoring/invalid})))]
                                 (let [error (failure #(authoring/write! host (principal w) (str (:room w)) base))]
                                   (is (= ::authoring/unavailable (:type (ex-data error))))
                                   (is (not (str/includes? (pr-str [(ex-message error) (ex-data error)]) "SECRET")))))
                               (let [can? access/can?]
                                 (with-redefs [access/can? (fn [db p action resource]
                                                             (and (not= :write action) (can? db p action resource)))]
                                   (is (= ::authoring/unavailable
                                          (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w)) base))))))))
                               (is (empty? @hydrations))))))

(deftest after-tree-limits-include-retained-files-and-utf8-byte-counts
  (@#'fixture/with-fixture true
                           (fn [{:keys [w host owned]}]
                             (seed! owned source/fixture)
                             (let [before (opened host w)
                                   size (reduce + 0 (map #(alength (.getBytes ^String % "UTF-8")) (vals source/fixture)))
                                   forbidden (fn [& _] (throw (AssertionError. "Over-limit AFTER dispatched")))]
                               (with-redefs [workflows/max-files 4 native/write-to-completion! forbidden]
                                 (is (= ::authoring/invalid
                                        (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w))
                                                                                    (request before {"fifth.md" "X"}))))))))
                               (with-redefs [workflows/max-bundle-bytes (inc size) native/write-to-completion! forbidden]
                                 (is (= ::authoring/invalid
                                        (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w))
                                                                                    (request before {"new.md" "λ"}))))))))))))

(deftest current-authority-and-original-target-are-rechecked-at-native-gates
  (@#'fixture/with-fixture true
                           (fn [{:keys [conn w host owned attached-descriptor]}]
                             (seed! owned source/fixture)
                             (let [before (opened host w) tree (query/worktree @owned)
                                   validate reads/validate-host-write! calls (atom 0)
                                   error (with-redefs [reads/validate-host-write!
                                                       (fn [& args]
                                                         (swap! calls inc)
                                                         (d/transact conn [{:room/id (:room w) :room/default-repository
                                                                            [:system/id (:registry-system attached-descriptor)]}])
                                                         (apply validate args))]
                                           (failure #(authoring/write! host (principal w) (str (:room w))
                                                                       (request before {"gold.edn" "Never written"}))))]
                               (is (= 1 @calls))
                               (is (= ::authoring/unavailable (:type (ex-data error))))
                               (is (= tree (query/worktree @owned)))))))

(deftest helper-failures-and-native-uncertainty-never-gain-conflict-or-replay-authority
  (@#'fixture/with-fixture true
                           (fn [{:keys [w host owned]}]
                             (seed! owned source/fixture)
                             (let [before (opened host w) input (request before {"gold.edn" "Edit"})]
                               (with-redefs [workflows/manifest-in (fn [& _] (throw (ex-info "SECRET" {:type ::authoring/conflict})))]
                                 (let [error (failure #(authoring/write! host (principal w) (str (:room w)) input))]
                                   (is (= ::authoring/unavailable (:type (ex-data error))))
                                   (is (not (str/includes? (pr-str [(ex-message error) (ex-data error)]) "SECRET")))))
                               (doseq [completion [:pending :not-started]]
                                 (let [calls (atom 0)]
                                   (with-redefs [native/write-to-completion! (fn [& _] (swap! calls inc)
                                                                               {:completion completion :error (ex-info "SECRET" {:type ::authoring/conflict})})]
                                     (let [error (failure #(authoring/write! host (principal w) (str (:room w)) input))]
                                       (is (= (if (= :pending completion) ::authoring/pending ::authoring/unavailable) (:type (ex-data error))))
                                       (is (= 1 @calls))
                                       (is (not (str/includes? (pr-str [(ex-message error) (ex-data error)]) "SECRET")))))))))))

(deftest real-noop-has-an-original-native-ack-and-the-same-owned-source
  (@#'fixture/with-fixture true
                           (fn [{:keys [w host owned]}]
                             (seed! owned source/fixture)
                             (let [before (opened host w)
                                   result (authoring/write! host (principal w) (str (:room w))
                                                            (request before {"gold.edn" (get source/fixture "gold.edn")}))]
                               (is (true? (get-in result [:write :applied?])))
                               (is (nil? (get-in result [:write :commit])))
                               (is (= before (:source result)))
                               (is (= (get (query/worktree @owned) "workflows/echo/checker.clj")
                                      (get-in result [:write :subtree :entries "checker.clj"])))))))

(deftest native-gate-membership-revocation-does-not-change-target-or-source
  (@#'fixture/with-fixture false
                           (fn [{:keys [conn w host owned]}]
                             (seed! owned source/fixture)
                             (let [before (opened host w) tree (query/worktree @owned) refs (query/refs @owned)
                                   validate reads/validate-host-write! calls (atom 0)
                                   error (with-redefs [reads/validate-host-write!
                                                       (fn [& args]
                                                         (swap! calls inc)
                                                         (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                                                         (apply validate args))]
                                           (failure #(authoring/write! host (principal w) (str (:room w))
                                                                       (request before {"gold.edn" "Denied edit"}))))]
                               (is (= 1 @calls))
                               (is (false? (access/can? @conn (:member w) :write {:room (:room w)})))
                               (is (= ::authoring/unavailable (:type (ex-data error))))
                               (is (= tree (query/worktree @owned)))
                               (is (= refs (query/refs @owned)))
                               (is (= (:revision before)
                                      (get-in (owned/details! host (ops/party-principal (:owner w)) (str (:room w)) {:name "echo"}) [:data :revision])))))))

(deftest causal-interruption-is-preserved-before-any-test-report
  (@#'fixture/with-fixture true
                           (fn [{:keys [w host owned]}]
                             (seed! owned source/fixture)
                             (let [before (opened host w) calls (atom 0)]
                               (try
                                 (let [error (with-redefs [workflows/manifest-in
                                                           (fn [& _] (swap! calls inc)
                                                             (throw (ex-info "SECRET/wrapped interruption" {}
                                                                             (InterruptedException. "Native interrupted"))))]
                                               (failure #(authoring/write! host (principal w) (str (:room w))
                                                                           (request before {"gold.edn" "Edit"}))))
               ;; clojure.test/report can consume the interrupt via STM. Record
               ;; it IMMEDIATELY before ANY is/report instead of testing later.
                                       interrupted? (.isInterrupted (Thread/currentThread))]
                                   (is interrupted?)
                                   (is (= 1 @calls))
                                   (is (= ::authoring/unavailable (:type (ex-data error))))
                                   (is (not (str/includes? (pr-str [(ex-message error) (ex-data error)]) "SECRET"))))
                                 (finally (Thread/interrupted)))))))

(deftest malformed-positive-subtree-witnesses-stay-pending-without-replay
  (@#'fixture/with-fixture true
                           (fn [{:keys [w host owned]}]
                             (seed! owned source/fixture)
                             (let [before (opened host w)]
                               (doseq [witness [nil {:prefix "/workflows/echo/" :entries {}}]]
                                 (let [calls (atom 0)
                                       error (with-redefs [native/write-to-completion!
                                                           (fn [& _] (swap! calls inc)
                                                             {:completion :returned :result {:applied? true :subtree witness}})]
                                               (failure #(authoring/write! host (principal w) (str (:room w))
                                                                           (request before {"gold.edn" "Edit"}))))]
                                   (is (= ::authoring/pending (:type (ex-data error))))
                                   (is (= 1 @calls))))))))

(deftest unsupported-original-regular-mode-refuses-before-loading-bodies
  (@#'fixture/with-fixture true
                           (fn [{:keys [w host owned]}]
                             (seed! owned source/fixture)
                             (d/transact owned [[:db/add [:geschichte.work/path "workflows/echo/gold.edn"] :geschichte.work/mode (long 33184)]])
                             (repo/stage-all! owned) (repo/commit! owned {:message "Unsupported regular permissions"})
                             (let [before (opened host w)]
                               (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Unsupported mode loaded body")))
                                             native/write-to-completion! (fn [& _] (throw (AssertionError. "Unsupported mode dispatched")))]
                                 (is (= ::authoring/invalid
                                        (:type (ex-data (failure #(authoring/write! host (principal w) (str (:room w))
                                                                                    (request before {"gold.edn" "Edit"}))))))))))))
