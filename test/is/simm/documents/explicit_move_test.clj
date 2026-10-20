(ns is.simm.documents.explicit-move-test
  "Real explicit native moves in isolated two-repository Rooms; default
   provisioning is the existing owned-workspace fixture's documented seam."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.rooms.files :as native]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.ops.owned-workspace-test :as fixture]))

(defn- with-fixture [owned-first? f]
  (let [configs (atom [])]
    (try
      (@#'fixture/with-fixture
       owned-first?
       (fn [{:keys [owned attached] :as data}]
         (reset! configs (mapv #(-> % deref :config) [owned attached]))
         (f data)))
      ;; The shared fixture physically closes its Room and releases these exact
      ;; isolated memory connections before this outer store deletion.
      (finally
        (doseq [cfg @configs]
          (when-not (and (= :memory (get-in cfg [:store :backend]))
                         (uuid? (get-in cfg [:store :id])))
            (throw (ex-info "Refuse non-fixture cleanup" {})))
          (d/delete-database cfg))))))

(deftest explicit-move-preserves-mode-and-only-mutates-its-exact-system
  (doseq [owned-first? [true false]]
    (with-fixture
      owned-first?
      (fn [{:keys [room owned attached own-descriptor]}]
        (let [system (:system own-descriptor)
              _ (native/write-to! room system {"/script.sh" {:text "echo exact\n"}}
                                  {:file-modes {"/script.sh" 33261}})
              before @owned other @attached expected (files/entry before "script.sh")
              selected (workspace/selected-system-id)
              write-to! native/write-to! calls (atom [])]
          (with-redefs [workspace/current-workspace (fn [] (throw (AssertionError. "Move read ambient workspace")))
                        workspace/select-workspace! (fn [& _] (throw (AssertionError. "Move changed selection")))
                        native/write! (fn [& _] (throw (AssertionError. "Move used ambient writer")))
                        native/write-to! (fn [r s batch options]
                                           (is (identical? system s))
                                           (swap! calls conj [batch options])
                                           (write-to! r s batch options))]
            (let [result (files/move-file-to! room system "script.sh" "src/script.sh" expected)]
              (is (true? (:moved? result)))
              (is (= expected (:entry result)))
              (is (= 33261 (get-in result [:entry :mode])))
              (is (nil? (files/entry @owned "script.sh")))
              (is (= "echo exact\n" (:text (files/read-file-in @owned "src/script.sh"))))))
          (is (= 1 (count @calls)))
          (is (= {"/script.sh" expected "/src/script.sh" nil}
                 (:expected-worktree (second (first @calls)))))
          (is (= {"/src/script.sh" 33261} (:file-modes (second (first @calls)))))
          (is (= other @attached))
          (is (= selected (workspace/selected-system-id))))))))

(deftest stale-source-and-occupied-target-refuse-before-body-or-native-writes
  (with-fixture
    true
    (fn [{:keys [room owned attached own-descriptor]}]
      (let [system (:system own-descriptor) old (files/entry @owned "wiki/file.md")]
        (repo/write! owned "wiki/file.md" (.getBytes "newer" "UTF-8"))
        (repo/write! owned "occupied.md" (.getBytes "occupied" "UTF-8"))
        (let [current (files/entry @owned "wiki/file.md") before @owned other @attached]
          (with-redefs [files/source-bytes (fn [& _] (throw (AssertionError. "Invalid move loaded a body")))
                        native/write-to! (fn [& _] (throw (AssertionError. "Invalid move reached writer")))]
            (is (thrown? Exception (files/move-file-to! room system "wiki/file.md" "new.md" old)))
            (is (thrown? Exception (files/move-file-to! room system "wiki/file.md" "occupied.md" current)))
            (is (thrown? Exception (files/move-file-to! room nil "wiki/file.md" "new.md" current))))
          (is (= before @owned))
          (is (= other @attached)))))))

(deftest native-batch-rechecks-races-and-the-original-authorization-gate
  (doseq [race [:source :target :denied]]
    (with-fixture
      false
      (fn [{:keys [room owned attached own-descriptor]}]
        (let [system (:system own-descriptor) expected (files/entry @owned "wiki/file.md")
              before @owned other @attached writer native/write-to! after-race (atom nil)
              authorize-calls (atom 0)]
          (with-redefs [native/write-to!
                        (fn [& args]
                          (case race
                            :source (repo/write! owned "wiki/file.md" (.getBytes "concurrent source" "UTF-8"))
                            :target (repo/write! owned "new.md" (.getBytes "concurrent target" "UTF-8"))
                            :denied nil)
                          (reset! after-race @owned)
                          (apply writer args))]
            (is (thrown? Exception
                         (files/move-file-to! room system "wiki/file.md" "new.md" expected
                                              {:authorize! #(do (swap! authorize-calls inc)
                                                                (when (= :denied race)
                                                                  (throw (ex-info "Current authority revoked" {}))))}))))
          (is (= @after-race @owned) "refusal adds no move/rollback transaction")
          (when (= :denied race)
            (is (pos? @authorize-calls))
            (is (= before @owned)))
          (is (= other @attached)))))))

(deftest move-receipt-is-the-original-complete-native-result-not-a-later-read
  (with-fixture
    true
    (fn [{:keys [room owned own-descriptor]}]
      (let [system (:system own-descriptor) expected (files/entry @owned "wiki/file.md")
            writer native/write-to!
            result (with-redefs [native/write-to!
                                 (fn [& args]
                                   (let [receipt (apply writer args)]
                                     (repo/write! owned "new.md" (.getBytes "later edit" "UTF-8"))
                                     receipt))]
                     (files/move-file-to! room system "wiki/file.md" "new.md" expected))]
        (is (= expected (:entry result)))
        (is (not= (:entry result) (files/entry @owned "new.md")))
        (is (= "later edit" (:text (files/read-file-in @owned "new.md"))))))))
