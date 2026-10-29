(ns is.simm.ops.proposal-evidence
  "Historical default-repository review, with App and physical Room ownership."
  (:require [datahike.api :as d]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.model.access :as access]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.semantic-diff :as diff]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- allowed? [db party room forks]
  (and (uuid? party) (uuid? room) (access/can? db party :read {:room room})
       (every? #(access/can? db party :read (:proposal.fork/scope %)) forks)))

(defn audit [host party id]
  (binding [ec/*execution-context* (:execution-ctx host)]
    (let [db (reads/current-host-control-db! host)
          p (when (uuid? id) (d/pull db '[* {:proposal/forks [* {:proposal.fork/checks [*]}]}]
                                     [:proposal/id id]))
          room-id (:proposal/room p)
          forks (filterv #(and (= :repo (:proposal.fork/system-type %))
                               (:proposal.fork/head-commit %)) (:proposal/forks p))]
      (when (and (seq forks) (allowed? db party room-id forks))
        (when-let [room (rooms/registered-room room-id)]
          (preparation/with-host-operation!
            room
            (fn [lease]
              (preparation/with-host-current! lease (constantly true))
              (let [source (admission/default-binding db room-id room)]
                ;; Room-agent code proposals own the Room's default repository.
                ;; Never borrow that lifetime for an unrelated attached store.
                (when (every? #(= (:registry-system source) (:proposal.fork/scope %)) forks)
                  (let [results (mapv (fn [f]
                                        (assoc (diff/semantic-repo-diff (:proposal.fork/scope f)
                                                                        (keyword (:proposal.fork/branch f))
                                                                        :system (:system source)
                                                                        :base-commit (:proposal.fork/base-commit f)
                                                                        :head-commit (:proposal.fork/head-commit f))
                                               :scope (str (:proposal.fork/scope f))
                                               :branch (:proposal.fork/branch f)
                                               :head-commit (:proposal.fork/head-commit f)
                                               :status (:proposal.fork/status f)
                                               :checks (into {} (map (fn [[k v]] [(keyword (name k)) v]))
                                                             (dissoc (:proposal.fork/checks f) :db/id)))) forks)
                        current-db (reads/current-host-control-db! host)
                        current (admission/default-binding current-db room-id room)]
                    (preparation/with-host-current! lease (constantly true))
                    (when (and (allowed? current-db party room-id forks)
                               (= (:registry-system source) (:registry-system current))
                               (identical? (:conn source) (:conn current)))
                      {:id (str id) :title (:proposal/title p) :summary (:proposal/summary p)
                       :status (:proposal/status p) :forks results})))))))))))
