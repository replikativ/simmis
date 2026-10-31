(ns is.simm.fixtures.accounting
  "Schema-only business accounting for service tests that never post to a book.
   Conserved Run resources still use the real native kernel and real wallets.
   Full provisioning/accounting tests must not use this substitution."
  (:require [datahike.api :as d]
            [is.simm.model.accounting :as accounting]
            [kontor.schema :as schema]))

(defn schema-only! [conn]
  (let [installed (d/schema @conn)
        missing (remove #(contains? installed (:db/ident %)) schema/all)]
    (when (seq missing) (d/transact conn (vec missing))))
  ;; Governance belongs to each fresh connection/owner even in a synthetic
  ;; fixture. Never share a governed live connection between cases.
  (accounting/govern-store! conn)
  conn)

(defn with-schema-only [f]
  ;; Scope ends when synchronous fixture construction has physically returned.
  ;; No test body, background task or sync/push operation runs under this
  ;; substitution; each receives real schemas and registered governors.
  (with-redefs [accounting/ensure-accounting-schema! schema-only!]
    (f)))
