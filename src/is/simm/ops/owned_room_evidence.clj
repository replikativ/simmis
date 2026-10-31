(ns is.simm.ops.owned-room-evidence
  "Fixed HOST pre-discovery original collection. Existing claims never resend."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.runtime.preparation :as preparation]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.model.room-evidence-collections :as collections]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.knowledge-program :as program]
            [is.simm.ops.conversation-capture :as capture]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.room-collection-policy :as policy]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Date]))

(def max-intent-bytes (* 4 1024 1024))
(defn- held! [reason] (throw (ex-info "Room evidence collection needs inspection" {:type ::held :reason reason})))
(defn- link [row]
  {:collection (:room.collection/id row) :intent (:room.collection/intent-id row)
   :proof (:room.collection/proof row)})
(defn- present! [db destinations]
  (doseq [{:keys [path text mode]} destinations]
    (let [entry (files/entry db path)]
      (when-not entry (held! :missing-or-partial-destinations))
      (when-not (and (= mode (:mode entry)) (= text (:text (files/read-file-in db path))))
        (held! :destination-conflict))))
  true)

(defn- scheduled-current-in!
  "The fixed claimed-occurrence gate, including pause-after-claim semantics."
  [db invocation by room]
  (when (:activation invocation)
    (let [{:keys [activation revision occurrence claim]} invocation
          actual (d/entity db [:schedule.occurrence/id occurrence])
          active (d/entity db [:schedule.activation/id activation])]
      (when-not (and (every? uuid? [activation revision occurrence claim])
                     (= by (:schedule.activation/by active)) (= room (:schedule.activation/room active))
                     (= claim (:schedule.occurrence/claim actual))
                     (= :admitting (:schedule.occurrence/state actual))
                     (= activation (:schedule.occurrence/activation actual))
                     (= revision (:schedule.occurrence/revision actual) (:schedule.activation/revision active))
                     (= occurrence (:schedule.activation/active-occurrence active)))
        (held! :schedule-claim-changed))))
  true)

(defn collect!
  "Called ONLY after exact saved source/model/resource readiness, within the
   original App operation. REQUEST is the future canonical UUID, never inserted
   here. Existing receipt recovery verifies retained bytes and never writes."
  [host principal handle original-room {:keys [request name revision bundle invocation] :as input}]
  (when-not (and (uuid? request) (uuid? revision) (uuid? bundle) (map? invocation))
    (held! :invalid-invocation))
  (lifetime/check-current!)
  (preparation/with-host-operation!
   (:room original-room) {:exclusive-key [::collection request]}
   (fn [lease]
     (binding [ec/*execution-context* (:ctx (:room original-room))]
       (let [control (get-in host [::reads/owner :control-conn])
             binding (reads/resolve-host-write! host principal handle)
             _ (when-not (identical? (:room original-room) (:room binding)) (held! :room-changed))
             mapping (owned/control-revision @control (:room-id binding) (get-in binding [:target :registry-system]))
             ticket lifetime/*ticket*
             check! #(do (lifetime/check-current!)
                         (preparation/with-host-current! lease (constantly true))
                         (reads/validate-host-write! host principal handle binding)
                         (scheduled-current-in! (reads/current-host-control-db! host) invocation
                                                (:party binding) (:room-id binding))
                         (let [fresh (reads/resolve-host-read! host principal handle)
                               manifest (workflows/manifest-in (:db fresh) name)]
                           (when-not (= revision (sources/source-id fresh name manifest)) (held! :program-changed))))
             transact! (fn [fragment]
                         (d/transact control
                           [[:db.fn/call (fn [db]
                                          (discovery/writer-current! host principal handle original-room lease ticket db)
                                          (owned/validate-control-in! db principal (:room-id binding) binding mapping)
                                          (scheduled-current-in! db invocation (:party binding) (:room-id binding))
                                          (fragment db))]]))
             store (artifact/datahike-store control)
             old (collections/receipt @control request)]
         (check!)
         (if old
           (let [intent (:value (artifact/get-bounded-value store (:room.collection/intent old) max-intent-bytes))]
             (when-not (and (= (:room.collection/intent-id old) (hasch/uuid intent))
                            (= [request name revision bundle invocation (:party binding) (:room-id binding)
                                (:target binding) (:target-stamp binding) mapping]
                               (mapv intent [:request :name :revision :bundle :invocation :actor :source-room :target :target-stamp :mapping])))
               (held! :retained-intent-mismatch))
             ;; Recovery promises availability only. Missing/partial remains held
             ;; until a host explicitly fences/joins the original worker.
             (present! (:db binding) (:destinations intent))
             (check!)
             (when-not (= :available (:room.collection/state old))
               (transact! #(collections/available-tx % old
                                                   (if (:room.collection/native-ack old) :write-acknowledged :observed-present)
                                                   nil (Date.))))
             {:status :available :receipt (link (collections/receipt @control request))})
           (let [source (capture/room-source binding)
                 plan (capture/room-plan binding source)]
             (when (= :held (:state plan)) (held! :source-held))
             (let [destinations (mapv #(assoc (select-keys % [:path :text]) :expected nil :mode 33188) (:artifacts plan))
                   intent {:version 1 :request request :name name :revision revision :bundle bundle
                           :invocation invocation :actor (:party binding) :source-room (:room-id binding)
                           ;; The scan is bounded by count, not transcript bytes.
                           ;; Retain only this batch's originals, never all later
                           ;; messages merely inspected to select the batch.
                           :source-store (:source-store source)
                           :source (capture/selected-room-source source (:artifacts plan))
                           :plan plan :policy policy/policy
                           :target (:target binding) :target-stamp (:target-stamp binding) :mapping mapping
                           :destinations destinations}
                   token (random-uuid)
                   _ (when (> (alength (.getBytes (pr-str intent) "UTF-8")) max-intent-bytes)
                       (held! :intent-size-limit))]
               (artifact/publish-value! store intent
                 (fn [ref] (check!) (transact! #(collections/claim-tx % intent ref token (Date.)))))
               (let [{:keys [receipt winner?]} (collections/claim-result @control request token)]
                 (when-not winner? (held! :claim-not-owned))
                 (let [authorize! #(do (check!)
                                      (when-not (= (capture/selected-room-source source (:artifacts plan))
                                                   (capture/selected-room-source
                                                    (capture/room-source (assoc binding :control-db (reads/current-host-control-db! host)))
                                                    (:artifacts plan)))
                                        (held! :source-changed)) true)
                       _ (authorize!)
                       ack (when (seq destinations)
                             (files/save-files-to! (:room binding) (:system binding)
                                                  (mapv #(dissoc % :mode) destinations)
                                                  {:authorize! authorize!}))]
                   (check!)
                   ;; Retain the obtained native ACK independently of availability
                   ;; verification, so a later inspection cannot erase known ACK.
                   (when ack
                     (artifact/publish-value! store ack
                       (fn [ref] (check!)
                         (transact! #(collections/acknowledge-tx % receipt ref (Date.))))))
                   ;; One newly published immutable Memory snapshot, all selected
                   ;; destinations, exact bytes AND mode; native ACK is separate.
                   (let [published (reads/resolve-host-read! host principal handle)]
                     (present! (:db published) destinations))
                   (let [acknowledged (collections/receipt @control request)]
                     (transact! #(collections/available-tx % acknowledged
                                                         (if ack :write-acknowledged :observed-present)
                                                         nil (Date.))))
                   (check!)
                   {:status :available :receipt (link (collections/receipt @control request))}))))))))))


(defn prepare!
  "Fixed opt-in preparation before discovery. An existing active knowledge
   review terminates this invocation as busy, without collection or discovery."
  [host principal handle original-room snapshot input invocation canonical-request epoch]
  (when (policy/declaration (:files snapshot))
    (let [current (room-operation/with-current!
                   host principal handle original-room
                   (fn []
                     (let [binding (reads/resolve-host-read! host principal handle)
                           described (program/describe (:party binding) (:room-id binding) (:target binding)
                                                       snapshot (:name input) (:expected-bundle input) epoch)
                           control (get-in host [::reads/owner :control-conn])]
                       (discovery/active-batch-reply @control (:value described)
                                                     (streams/stream @control (get-in described [:value :id]))))))]
      (if current
        current
        (:receipt (collect! host principal handle original-room
                            {:request canonical-request :name (:name input)
                             :revision (:expected-owned-source input) :bundle (:expected-bundle input)
                             :invocation invocation}))))))
