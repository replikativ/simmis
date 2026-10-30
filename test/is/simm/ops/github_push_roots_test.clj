(ns is.simm.ops.github-push-roots-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [geschichte.git.object :as object]
            [geschichte.repo :as repo]
            [is.simm.model.github-push :as push]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.checkpoint-roots :as physical]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-push-roots :as roots]))

(defn- with-retention [f]
  (let [configuration #(hash-map :store {:backend :memory :id (random-uuid)}
                                 :schema-flexibility :write :keep-history? true :commit-graph? true)
        source-config (configuration) control-config (configuration)]
    (d/create-database source-config) (d/create-database control-config)
    (let [source (d/connect source-config) control (d/connect control-config)]
      (try
        (repo/init! source)
        (repo/write! source "file.txt" (object/utf8 "Saved outbound content\n"))
        (repo/stage-all! source)
        (let [head (:geschichte.commit/id (repo/commit! source {:message "Authored head"}))
              oid (apply str (repeat 40 "a"))
              _ (d/transact source [[:db/add [:geschichte.commit/id head] :geschichte.commit/git-oid oid]])
              _ (d/transact control (into journal/schema push/schema))
              by (random-uuid) room (random-uuid) client (random-uuid)
              binding {:id (journal/request-id by room client) :by by :room room :client-id client :claim (random-uuid)}
              _ (artifact/publish-value! (artifact/datahike-store control) {:test :preparation}
                                         #(d/transact control [[:db.fn/call push/prepare-tx binding % (random-uuid)]]))
              creating (journal/sync-in @control (:id binding))
              body (roots/describe @source creating head oid)
              report (volatile! nil)
              _ (artifact/publish-value! (artifact/datahike-store control) body
                                         #(vreset! report (d/transact control [[:db.fn/call push/transition-tx creating :retaining % (roots/body-id body)]])))
              row (journal/sync-in @control (:id binding)) context {:conn control}]
          (with-redefs [host/current! (fn [supplied] (assert (identical? context supplied)) {:db @source})]
            (f {:source source :control control :context context :row row :body body :report @report})))
        (finally
          (d/release control) (d/release source)
          (d/delete-database control-config) (d/delete-database source-config))))))

(deftest sent-snapshot-and-postmapping-source-are-distinct-retained-roots
  (with-retention
    (fn [{:keys [source context row body report]}]
      (is (not= (get-in body [:roots :sent :basis :commit]) (get-in body [:roots :source :basis :commit])))
      (is (thrown? clojure.lang.ExceptionInfo (roots/verify! context row body)))
      (is (thrown? clojure.lang.ExceptionInfo (roots/create-original! context row body nil)))
      (is (empty? (physical/registry! @source)))
      (is (= body (roots/create-original! context row body report)))
      (is (= 2 (count (physical/registry! @source))))
      (is (= body (roots/verify! context row body)))
      (is (thrown? clojure.lang.ExceptionInfo (roots/create-original! context row body report)))
      (is (= body (roots/verify! context row body))))))

(deftest partial-retention-recovery-never-creates-the-missing-root
  (with-retention
    (fn [{:keys [source context row body report]}]
      (let [create physical/create! calls (atom 0)]
        (with-redefs [physical/create! (fn [& args]
                                         (if (= 2 (swap! calls inc))
                                           (throw (ex-info "Second pin unavailable" {}))
                                           (apply create args)))]
          (is (thrown? clojure.lang.ExceptionInfo (roots/create-original! context row body report)))
          (is (= 1 (count (physical/registry! @source))))
          (is (thrown? clojure.lang.ExceptionInfo (roots/verify! context row body)))
          (is (= 2 @calls)))))))
