(ns is.simm.model.system-db-schema-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.model.benchmark-resolutions :as resolutions]
            [is.simm.model.github-sync :as github]
            [is.simm.model.system-db :as system-db]))

(deftest production-control-schema-includes-benchmark-evidence
  (let [installed (into {} (map (juxt :db/ident identity)) system-db/schema)]
    (doseq [attribute resolutions/schema]
      (is (= attribute (get installed (:db/ident attribute)))
          (str "Production startup must install " (:db/ident attribute))))))

(deftest production-control-schema-includes-github-source-and-sync-ownership
  (let [installed (into {} (map (juxt :db/ident identity)) system-db/schema)]
    (doseq [attribute github/schema]
      (is (= attribute (get installed (:db/ident attribute)))))
    (doseq [ident [:room/github-source :room/github-ref :room/github-import-head :room/github-repository]]
      (is (contains? installed ident)))))
