(ns is.simm.runtimes.branch-gc-test
  "The lazy branch sweep runs on a store's first selection after boot, when a
   context forked since may already be working on an overlay branch. It must
   delete the orphans a previous process left, and nothing a live context uses."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.versioning :as dv]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]
            [konserve.core :as k]))

(deftest the-sweep-spares-a-branch-a-live-context-uses
  (let [scope (random-uuid)
        cfg {:store {:backend :memory :id scope}
             :schema-flexibility :read :keep-history? true :commit-graph? true}
        _ (d/create-database cfg)
        conn (d/connect cfg)
        live (keyword (str "overlay-" (random-uuid)))
        orphan (keyword (str "overlay-" (random-uuid)))]
    (try
      (d/transact conn [{:name "x"}])
      (dv/branch! conn :db live)
      (dv/branch! conn :db orphan)
      (let [live-conn (d/connect (assoc cfg :branch live))]
        (try
          (branching/register-system! conn scope)
          (is (= 1 (ctx/with-server-context (branching/gc-internal-branches! scope))) "only the orphan")
          (let [branches (k/get (:store @conn) :branches nil {:sync? true})]
            (is (contains? branches live))
            (is (not (contains? branches orphan)))
            (is (contains? branches :db)))
          (is (= [{:name "x"}] (d/q '[:find [(pull ?e [:name]) ...] :where [?e :name _]] @live-conn))
              "the live branch's connection still works")
          (finally (d/release live-conn))))
      (finally
        (d/release conn)
        (d/delete-database cfg)))))
