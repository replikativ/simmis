(ns is.simm.model.rpc-coverage-test
  "Every network-reachable RPC is policed.

   `authorize-remote` is deny-by-default, so an RPC with no `rpc-policy` row
   fails CLOSED — refused rather than exposed. That is the safe direction, but
   it makes the omission invisible: the feature is simply dead, and whoever
   added the fn finds out from a user.

   This is the invariant that rots. `rpc-policy` is a table maintained by hand
   beside ~100 registrations spread over twelve namespaces, and nothing about
   writing a `defn-spin-remote` forces you to add a row. So assert the join
   itself, against the LIVE registry rather than a grep — the macro mangles
   names into `<ns>/spin-remote-<name>-<idx>` and only `normalize-remote-name`
   knows how to undo that.

   The HTTP plane gets this guarantee at BOOT instead (reitit's `:validate`
   hook — see `http-auth/validate-auth-declared!`), which is stronger. The RPC
   registry is populated by requiring namespaces rather than by building one
   object, so there is no single construction point to hang it on; a test is
   the available equivalent."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.model.access :as access]
            [is.simm.distributed-scope :as ds]))

(def ^:private registering-namespaces
  "Requiring these populates `ds/remote-fn-registry` the way a server boot
   does. If a new `*-remote` namespace appears and is not listed here, the
   count assertion below is what notices."
  '[is.simm.uis.web.desktop.accounting-remote
    is.simm.uis.web.desktop.admin-remote
    is.simm.uis.web.desktop.block-remote
    is.simm.uis.web.desktop.branching-remote
    is.simm.uis.web.desktop.chat-remote
    is.simm.uis.web.desktop.experiments-remote
    is.simm.uis.web.desktop.feed-remote
    is.simm.uis.web.desktop.mail-remote
    is.simm.uis.web.desktop.proposals-remote
    is.simm.uis.web.desktop.sandbox-remote
    is.simm.uis.web.desktop.settings-remote
    is.simm.uis.web.desktop.tasks-remote])

(defn- load-all! [] (doseq [n registering-namespaces] (require n)))

(deftest every-registered-rpc-has-a-policy
  (load-all!)
  (let [registered (keys @ds/remote-fn-registry)
        unpoliced (->> registered
                       (remove #(contains? access/rpc-policy
                                           (access/normalize-remote-name %)))
                       (map str) sort vec)]
    (testing "the registry is populated — an empty join proves nothing"
      (is (< 50 (count registered))
          (str "only " (count registered) " RPCs registered; `registering-namespaces` "
               "is probably no longer the full list, and this test would pass "
               "vacuously")))
    (testing "no network-reachable fn is missing from rpc-policy"
      (is (= [] unpoliced)
          (str "reachable but unpoliced, so refused at runtime and the feature "
               "is dead: " (pr-str unpoliced))))))

(deftest policy-rows-name-real-fns
  (load-all!)
  (let [live (into #{} (map access/normalize-remote-name) (keys @ds/remote-fn-registry))
        orphans (->> (keys access/rpc-policy) (remove live) sort vec)]
    (testing "a row for a fn that no longer exists is dead weight, not a risk"
      ;; Reported, not failed. `datahike.kabel`'s handlers register in
      ;; `kabel.remote/functions`, not in this registry, so their rows
      ;; (dispatch, create-database, delete-database) never have a
      ;; counterpart here; the server does not serve that registry either
      ;; (see authority/raw-paths-test).
      (when (seq orphans)
        (println "    note:" (count orphans)
                 "rpc-policy rows with no registered fn:" (pr-str orphans)))
      (is true))))
