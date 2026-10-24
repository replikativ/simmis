(ns is.simm.ops.workflow-readiness
  "Read-only workflow choices and the actual shared Room wallet observation."
  (:require [clojure.string :as str]
            [dvergr.model.registry :as registry]
            [dvergr.resource :as resource]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.model-catalog :as catalog]
            [is.simm.model.model-selection :as selection]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.work :as work]
            [is.simm.runtimes.application-ops :as application]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.math BigDecimal BigInteger]))

(defn- refuse! []
  (throw (ex-info "Workflow readiness unavailable" {:type ::unavailable})))

(defn exact-model-options
  "Local registered metadata and credential presence only. No catalog refresh,
   provider initialization, token refresh, model request or implicit selection."
  []
  (let [models (->> (registry/list-models)
                    (filter (fn [{:keys [id provider api-type]}]
                              (and (string? id) (<= 1 (count id) 200)
                                   (not (str/blank? id)) (not (re-find #"[\p{Cntrl}]" id))
                                   (contains? work/dispatch-accounted-providers provider)
                                   (contains? (:implemented-api-types (selection/provider-contract provider)) api-type))))
                    (sort-by :id) vec)
        present (into {} (map (fn [provider] [provider (selection/credential-present? provider)]))
                      (set (map :provider models)))]
    {:models
     (mapv (fn [{:keys [id provider api-type]}]
             (let [label (str (catalog/model-label id))]
              {:id id :provider provider :api-type api-type
              :label (subs label 0 (min 200 (count label)))
              :provider-label (catalog/provider-label provider)
              :credential-status (if (get present provider) :present :undetected)
              :credential-label (if (get present provider)
                                  (if (= :codex-subscription provider) "Login present; connection not tested"
                                      "Credential configured; account access not checked")
                                  "Default credential not detected; connection not verified")}))
           (take 256 models))
     :models-truncated? (> (count models) 256)}))

(defn- exact-quantity [value]
  (let [value (if (nil? value) 0 value)
        ;; Bound before decimal expansion or printing. A compact 1E+1000000000
        ;; must never allocate its billion-digit integer during a metadata read.
        integer (cond
                  (integer? value) (biginteger value)
                  (instance? BigDecimal value)
                  (let [^BigDecimal decimal value
                        _ (when (> (.bitLength (.unscaledValue decimal)) 1701) (refuse!))
                        precision (.precision decimal)
                        scale (long (.scale decimal))]
                    (when (neg? (.signum decimal)) (refuse!))
                    (if (zero? (.signum decimal))
                      BigInteger/ZERO
                      (do
                        (when (or (> precision 512)
                                  (> (- (long precision) scale) 512)
                                  (>= scale precision))
                          (refuse!))
                        (.toBigIntegerExact decimal))))
                  :else (refuse!))]
    (when (or (neg? (.signum ^BigInteger integer))
              (> (.bitLength ^BigInteger integer) 1701))
      (refuse!))
    (let [text (str integer)]
      (when (> (count text) 512) (refuse!))
      text)))

(defn- allowance [room]
  (try
    (binding [ec/*execution-context* (:ctx room)]
      (let [balance (resource/balance room)]
        (when-not (map? balance) (refuse!))
        {:status :observed
         :microdollars (exact-quantity (get balance resource/microdollars))
         :model-dispatches (exact-quantity (get balance resource/model-dispatches))}))
    (catch clojure.lang.ExceptionInfo error
      (if (contains? #{:dvergr.resource/resource-store-unavailable :kontor.resource/account-not-found}
                     (:type (ex-data error)))
        {:status :unavailable :reason :wallet-not-configured}
        (throw error)))))

(defn read-on-host!
  "HOST internal. Authorization precedes metadata; exact Room/App lifetimes and
   current authority are checked again before publishing a closed DTO."
  [host principal handle]
  (let [original (reads/resolve-registered-host-read-room! host principal handle)
        room (:room original)
        observe (fn []
                  (let [models (exact-model-options)
                        balance (if room (allowance room) {:status :unavailable :reason :room-not-active})
                        current (reads/resolve-registered-host-read-room! host principal handle)]
                    (when-not (and (= (select-keys original [:party :room-id :slug])
                                      (select-keys current [:party :room-id :slug]))
                                   (identical? room (:room current))) (refuse!))
                    (when room (reads/validate-host-read-room! host principal handle original))
                    (assoc models :allowance balance)))]
    (binding [ec/*execution-context* (:execution-ctx host)]
      (if room
        (preparation/with-host-operation!
          room (fn [lease]
                 (preparation/with-host-current! lease (constantly true))
                 (reads/validate-host-read-room! host principal handle original)
                 (let [result (observe)]
                   (preparation/with-host-current! lease (constantly true))
                   result)))
        (observe)))))

(defn- sanitized [body]
  (try (body)
       (catch Throwable error
         (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
         (if (= ::reads/refused (:type (ex-data error)))
           (throw (ex-info "Room permission required" {:type :not-authorized}))
           (refuse!)))))

(defn prepare-readiness
  "Capture original App BEFORE queuing this fixed human metadata read."
  [party handle]
  (sanitized
   (fn []
     (when-not (and (uuid? party) (string? handle) (<= 1 (count handle) 200)) (refuse!))
     (let [operation (application/capture-operation)
           principal {:principal/kind :human :principal/id (str "party/" party)}]
       (fn [] (sanitized #(operation (fn [host] (read-on-host! host principal handle)))))))))
