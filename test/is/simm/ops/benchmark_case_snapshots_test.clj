(ns is.simm.ops.benchmark-case-snapshots-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.versioning :as versioning]
            [geschichte.repo :as repo]
            [is.simm.ops.benchmark-case-snapshots :as cases]
            [is.simm.ops.checkpoint-roots :as physical]
            [is.simm.ops.knowledge-changes :as changes]))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn {:name "Private benchmark fixture"})
        (f conn (changes/basis @conn))
        (finally (d/release conn) (d/delete-database cfg))))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(deftest retired-builder-restores-exact-case-and-uses-unchanged-pin-verification
  (fixture
   (fn [conn c0]
     (let [branch (keyword (str "case-" (random-uuid))) root (random-uuid)
           owner {:case (random-uuid) :claim (random-uuid)}]
       (versioning/branch! conn (get-in @conn [:config :branch]) branch)
       (let [builder (d/connect (assoc (:config @conn) :branch branch))
             basis (try
                     (d/transact builder [{:db/id -1 :db/ident :benchmark.case/visible}])
                     (let [basis (changes/basis @builder)]
                       (physical/create! @builder basis root owner)
                       basis)
                     (finally (d/release builder)))]
         (versioning/delete-branch! conn branch)
         (let [restored (cases/load-case-snapshot! @conn c0 basis)
               verified (physical/verify! restored basis root owner)]
           (is (= basis (changes/basis restored)))
           (is (not= (:branch c0) (:branch basis)))
           (is (= basis (:basis verified)))
           (is (d/q '[:find ?e . :where [?e :db/ident :benchmark.case/visible]] restored))
           (is (nil? (d/q '[:find ?e . :where [?e :db/ident :benchmark.case/visible]] @conn)))
           (is (= :pin-mismatch
                  (reason #(physical/verify! restored basis root (assoc owner :claim (random-uuid))))))))))))

(deftest unchanged-case-can-retain-c0-without-a-synthetic-builder-identity
  (fixture
   (fn [conn c0]
     (is (= c0 (changes/basis (cases/load-case-snapshot! @conn c0 c0)))))))

(deftest wrong-fixture-or-case-store-is-refused-before-case-commit-read
  (fixture
   (fn [conn c0]
     (let [lookup d/commit-as-db source-store (:store @conn)]
       (with-redefs [d/commit-as-db
                     (fn [source commit]
                       ;; C0 publication validation legitimately reads through
                       ;; its DB. Only a raw-store CASE lookup is forbidden.
                       (when (identical? source-store source)
                         (throw (AssertionError. "Wrong case source must not be read")))
                       (lookup source commit))]
         (is (= :fixture-changed
                (reason #(cases/load-case-snapshot! @conn (assoc c0 :commit (random-uuid)) c0))))
         (doseq [[key value] [[:store (random-uuid)] [:repository (random-uuid)] [:backend :file]]]
           (is (= :case-store-changed
                  (reason #(cases/load-case-snapshot! @conn c0 (assoc c0 key value)))))))))))

(deftest self-consistent-location-is-insufficient-without-the-complete-stored-basis
  (fixture
   (fn [conn c0]
     (doseq [[key value] [[:configuration (random-uuid)] [:branch :different-builder]
                         [:max-tx (inc (:max-tx c0))] [:datoms (inc (:datoms c0))]]]
       (is (= :case-basis-changed
              (reason #(cases/load-case-snapshot! @conn c0 (assoc c0 key value)))))))))
