(ns is.simm.ops.file-output-bound-dispatch-test
  "Serialized controller admission tests, not native target publication."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-dispatch-test :as fixture]
            [is.simm.ops.owned-workspace :as owned]))

(defn- apply-bound! [{:keys [conn party room-id room claimed]} binding]
  (adoption/bound-applying! conn party room-id room claimed
                            (@#'fixture/candidate claimed) binding))

(deftest original-host-binding-is-checked-against-the-serialized-writer-db
  (@#'fixture/with-claimed
   (fn [{:keys [conn party room-id room claimed workspace] :as state}]
     (let [binding {:target (get-in claimed [:proof :binding :target])}
           transact d/transact checked (atom []) before @workspace]
       (with-redefs [d/transact
                     (fn [& args]
                       ;; This occurs AFTER the caller captured its binding but
                       ;; BEFORE the native applying transaction is submitted.
                       (transact conn [{:room/id room-id :room/name "Writer-snapshot marker"}])
                       (apply transact args))
                     owned/validate!
                     (fn [db principal action rid canonical original]
                       (swap! checked conj (:room/name (d/entity db [:room/id room-id])))
                       (is (= party principal))
                       (is (= :merge action))
                       (is (= room-id rid))
                       (is (identical? room canonical))
                       (is (identical? binding original))
                       binding)]
         (let [result (apply-bound! state binding)]
           (is (= :applying (get-in result [:operation :state])))
           (is (= (:operation result)
                  (adoptions/operation @conn (get-in claimed [:operation :id]))))))
       (is (= ["Writer-snapshot marker"] @checked))
       (is (= before @workspace))))))

(deftest refused-or-retargeted-owned-binding-cannot-advance-the-journal
  (doseq [mode [:denied :changed-target]]
    (@#'fixture/with-claimed
     (fn [{:keys [conn claimed workspace] :as state}]
       (let [before @workspace binding {:target (get-in claimed [:proof :binding :target])}
             checks (atom 0)]
         (with-redefs [owned/validate!
                       (fn [& _]
                         (swap! checks inc)
                         (case mode
                           :denied (throw (ex-info "Binding replaced or permission revoked" {}))
                           :changed-target (assoc-in binding [:target :store] (random-uuid))))]
           (is (thrown? clojure.lang.ExceptionInfo (apply-bound! state binding))))
         (is (= 1 @checks))
         (is (= :claimed (:state (adoptions/operation @conn (get-in claimed [:operation :id])))))
         (is (= before @workspace)))))))

(deftest missing-host-binding-refuses-before-submitting-a-transaction
  (@#'fixture/with-claimed
   (fn [state]
     (with-redefs [d/transact (fn [& _] (throw (AssertionError. "Missing binding reached writer")))]
       (is (thrown? clojure.lang.ExceptionInfo (apply-bound! state nil)))))))
