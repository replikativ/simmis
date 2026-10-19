(ns is.simm.ops.file-output-storage-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.gc-guard :as guard]
            [datahike.store :as ds]
            [dvergr.artifact :as artifact]
            [geschichte.content :as content]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.file-output-plan-test :as fixture]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-outputs :as outputs]
            [superv.async :refer [<?? S]]))

(defn- with-store [f]
  (let [parent (.toString (java.nio.file.Files/createTempDirectory
                           "simmis-file-output-control-" (make-array java.nio.file.attribute.FileAttribute 0)))
        path (str parent "/store")
        cfg {:store {:backend :file :path path :id (random-uuid)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn manifests/schema)
        (d/transact conn [{:db/ident :fixture/id :db/valueType :db.type/uuid
                          :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                         {:db/ident :fixture/manifest :db/valueType :db.type/ref
                          :db/cardinality :db.cardinality/one :db/isComponent true}])
        (f conn)
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest missing-and-non-uuid-identities-cannot-select-an-arbitrary-manifest
  (with-redefs [d/datoms (fn [& _] (throw (AssertionError. "Invalid id reached index scan")))
                d/pull (fn [& _] (throw (AssertionError. "Invalid id reached pull")))]
    (doseq [id [nil "" (str (random-uuid)) :first 0]]
      (is (thrown? clojure.lang.ExceptionInfo (storage/manifest (atom nil) id))))))

(deftest file-evidence-is-owned-by-the-control-store-not-the-source-world
  (with-store
    (fn [conn]
      (let [saved (atom nil) orphan (atom nil)
            sid (ds/canonical-store-id (:store @conn) (get-in @conn [:config :store]))]
        ;; Unreferenced control proves the later sweep actually collects blobs.
        (artifact/publish-value! (artifact/datahike-store conn) {:orphan (random-uuid)} #(reset! orphan %))
        (@#'fixture/with-input
         (fn [source base]
           (let [plan (plans/plan (assoc base :output (@#'fixture/output! source {"wiki/old.md" "After" "wiki/new.md" "New"})))]
             (reset! saved plan)
             (storage/publish! conn plan
                               (fn [entity]
                                 (is (guard/in-flight? sid) "Body guards cover the referencing transaction")
                                 (d/transact conn [{:fixture/id (:proposal plan) :fixture/manifest entity}]))))))
        ;; Source repository has now closed. Neither projection nor body reads
        ;; may reach it, a Run capability, or the original Attempt.
        (<?? S (d/gc-storage conn (java.util.Date.) {:min-age-ms 0}))
        (is (nil? (artifact/get-value (artifact/datahike-store conn) @orphan)))
        (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Read source world after retention")))]
          (let [id (get-in @saved [:manifest :id])]
            (with-redefs [artifact/get-value (fn [& _] (throw (AssertionError. "Metadata loaded file bodies")))]
              (is (= (:manifest @saved) (storage/manifest conn id))))
            (let [get-value artifact/get-value calls (atom 0)
                  pair (with-redefs [artifact/get-value (fn [& args] (swap! calls inc) (apply get-value args))]
                         (storage/read-file conn id "wiki/old.md"))]
              (is (= 1 @calls) "One file fetch, not the complete output bundle")
              (is (= "Before\r\n" (markdown/decode (outputs/bytes! (:before pair)))))
              (is (= "After" (markdown/decode (outputs/bytes! (:after pair))))))
            (is (nil? (:before (storage/read-file conn id "wiki/new.md"))))
            (is (thrown? clojure.lang.ExceptionInfo (storage/read-file conn id "wiki/absent.md")))))
        (is (not (guard/in-flight? sid)) "No publication guard leaks after commit")))))

(deftest failed-referencing-transaction-leaves-no-durable-manifest
  (with-store
    (fn [conn]
      (@#'fixture/with-input
       (fn [source base]
         (let [plan (plans/plan (assoc base :output (@#'fixture/output! source {"wiki/new.md" "New"})))]
           (is (thrown? clojure.lang.ExceptionInfo
                        (storage/publish! conn plan (fn [_] (throw (ex-info "No pointer committed" {}))))))
           (is (nil? (d/q '[:find ?e . :in $ ?id :where [?e :file-output/id ?id]]
                           @conn (get-in plan [:manifest :id]))))
           (is (not (guard/in-flight? (ds/canonical-store-id (:store @conn) (get-in @conn [:config :store])))))))))))

(deftest tampered-typed-metadata-refuses-before-any-body-read
  (with-store
    (fn [conn]
      (@#'fixture/with-input
       (fn [source base]
         (let [plan (plans/plan (assoc base :output (@#'fixture/output! source {"wiki/new.md" "New"})))
               id (get-in plan [:manifest :id])]
           (storage/publish! conn plan #(d/transact conn [{:fixture/id (:proposal plan) :fixture/manifest %}]))
           (d/transact conn [[:db/add [:file-output/id id] :file-output/room (random-uuid)]])
           (with-redefs [artifact/get-value (fn [& _] (throw (AssertionError. "Corrupt metadata loaded bodies")))]
             (is (thrown? clojure.lang.ExceptionInfo (storage/read-file conn id "wiki/new.md"))))))))))

(deftest oversized-component-list-refuses-before-pulling-child-metadata
  (with-store
    (fn [conn]
      (@#'fixture/with-input
       (fn [source base]
         (let [plan (plans/plan (assoc base :output (@#'fixture/output! source {"wiki/new.md" "New"})))
               id (get-in plan [:manifest :id])]
           (storage/publish! conn plan #(d/transact conn [{:fixture/id (:proposal plan) :fixture/manifest %}]))
           (d/transact conn [{:db/id [:file-output/id id]
                             :file-output/entries (mapv (fn [n] {:file-output.entry/path (str "wiki/fake-" n ".md")})
                                                        (range 100))}])
           (with-redefs [d/pull (fn [& _] (throw (AssertionError. "Over-budget manifest reached pull")))]
             (is (thrown? clojure.lang.ExceptionInfo (storage/manifest conn id))))))))))
