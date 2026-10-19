(ns is.simm.ops.file-output-adoption
  "INTERNAL read-only preflight for forthcoming guarded adoption. No route,
   claim, schema installation, publication, status mutation or execution. The
   returned data is NOT a capability: an eventual serialized host writer must
   repeat authority/target checks and compare the exact frozen storage proof."
  (:require [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [malli.core :as m]))

(defn- refuse! [reason]
  (throw (ex-info "Retained file adoption is unavailable"
                  {:type ::unavailable :reason reason})))

(defn snapshot
  "INTERNAL pure control-DB read. Authorize before inspecting component data.
   Only a single ordinary open file component is supported. This neither loads
   bodies nor grants claim ownership; use against the serialized writer's DB
   again before performing any eventual journal transition."
  [db party room-id proposal-id]
  (when-not (and (every? uuid? [party room-id proposal-id])
                 (access/can? db party :read {:room room-id})
                 (access/can? db party :merge {:room room-id}))
    (refuse! :denied))
  (let [p (d/pull db [:db/id :proposal/id :proposal/room :proposal/status
                      :proposal/author :proposal/run :proposal/filing-hash :proposal/review]
                  [:proposal/id proposal-id])]
    (when-not (and (= proposal-id (:proposal/id p)) (= room-id (:proposal/room p))
                   (= :open (:proposal/status p)) (nil? (:proposal/review p)))
      (refuse! :unsupported-proposal))
    ;; Count before component pull: plain/wildcard component pull can allocate
    ;; every child before a caller discovers that the Proposal was oversized.
    (let [links (vec (take 2 (d/datoms db :eavt (:db/id p) :proposal/forks)))]
      (when-not (= 1 (count links)) (refuse! :unsupported-component-set))
      (let [fork (d/pull db [:db/id :proposal.fork/scope :proposal.fork/authority-scope
                             :proposal.fork/branch :proposal.fork/status :proposal.fork/system-type
                             {:proposal.fork/file-output [:file-output/id :file-output/room]}]
                         (:v (first links)))
            scope (:proposal.fork/scope fork)]
        (when-not (and (= :file-output (:proposal.fork/system-type fork))
                       (nil? (:proposal.fork/status fork))
                       (= room-id (:proposal.fork/authority-scope fork))
                       (string? (:proposal.fork/branch fork))
                       (re-matches #"[A-Za-z0-9._-]{1,128}" (:proposal.fork/branch fork))
                       (access/may-merge-proposal-fork? db party fork))
          (refuse! :unsupported-component))
        (let [owner (manifests/owner db scope)]
          (when-not (= (select-keys owner [:proposal :room :fork-eid :proposal-eid])
                       {:proposal proposal-id :room room-id :fork-eid (:db/id fork) :proposal-eid (:db/id p)})
            (refuse! :invalid-owner))
          (let [{:keys [manifest] :as retained} (storage/retained-in db scope)
                request (d/pull db '[*] [:work.request/id (:request manifest)])
                disposition (dispositions/disposition request)
                binding {:proposal proposal-id :component scope :manifest (:id manifest)
                         :room room-id :actor party :filing (:proposal/filing-hash p)
                         :disposition (:id disposition) :target (:target manifest)}]
            (when-not (and (= :proposed (:kind disposition))
                           (= proposal-id (:proposal disposition))
                           (= scope (:manifest disposition))
                           (= room-id (:room manifest) (:work.request/room request))
                           (= (:proposal/author p) (:work.request/by request))
                           (= (:proposal/run p) (:candidate disposition) (:attempt manifest))
                           (= (:request manifest) (:request disposition))
                           (= (:basis manifest) (:basis disposition))
                           (= (:output manifest) (:output disposition))
                           (seq (:entries manifest))
                           (publication/host-target? (:target manifest))
                           (m/validate adoptions/binding-schema binding))
              (refuse! :invalid-filing))
            {:binding binding :proposal p :fork fork :retained retained
             :disposition disposition}))))))

(defn prepare
  "Host-only read preflight for a NEW adoption. Canonical Room/default target
   verification precedes the first body fetch. Freeze all exact body refs from
   one DB; validate every bounded pair before returning. Concurrent deletion/GC
   refuses, never repairs/reexecutes. Retained evidence outlives the Attempt.
   Party is captured by the trusted authenticated host, never a client field.
   No caller-supplied bytes, manifest, descriptor or claim token is accepted."
  [conn party room-id room proposal-id]
  (when (simple/on-drain-thread?) (refuse! :requires-host-worker))
  (when-not (and conn (identical? conn (sdb/get-conn))) (refuse! :noncanonical-connection))
  (let [db @conn
        {:keys [binding retained] :as proof} (snapshot db party room-id proposal-id)
        manifest (:manifest retained)]
    (when-not (= (:target binding) (filing/current-target db room-id room))
      (refuse! :target-changed))
    (let [bodies (into (sorted-map)
                       (for [path (keys (:entries manifest))]
                         [path (storage/read-file-in conn db (:id manifest) path)]))]
      (plans/validate-plan! {:manifest manifest :proposal proposal-id :bodies bodies})
      {:proof proof :plan {:manifest manifest :proposal proposal-id :bodies bodies}})))
