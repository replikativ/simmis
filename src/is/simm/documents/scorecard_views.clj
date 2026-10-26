(ns is.simm.documents.scorecard-views
  "Bounded presentation from one native Scorecard summary and frozen definition.
   The caller authorizes the Room and uses the native bounded/certified reader.
   No artifact IO, stats reinterpretation, page projection or editing bindings."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.documents.experiment-provenance :as provenance]
            [is.simm.documents.saved-view-contract :as contract]
            [is.simm.documents.saved-view-spec :as spec])
  (:import [java.math RoundingMode]
           [java.nio CharBuffer]
           [java.nio.charset StandardCharsets CodingErrorAction]))

(def max-presentation-bytes 131072)

(defn- refuse! [reason]
  (throw (ex-info "Scorecard summary cannot be displayed" {:type ::refused :reason reason})))

(defn- finite-number! [value]
  (when-not (and (number? value) (Double/isFinite (double value)))
    (refuse! :unsupported-number))
  value)

(defn- number-text [value]
  (let [text (str (finite-number! value))]
    (when (> (count text) 1024) (refuse! :oversized-number))
    text))

(defn- percent-text [value]
  (finite-number! value)
  (when (and (instance? java.math.BigDecimal value)
             (not (<= -1024 (.scale ^java.math.BigDecimal value) 1024)))
    (refuse! :oversized-number))
  (.toPlainString (.stripTrailingZeros
                   (.setScale (.movePointRight (bigdec (str value)) 2) 1 RoundingMode/HALF_UP))))

(defn- number-cell [value & [unit]]
  (if (nil? value) {:kind :null}
      (cond-> {:kind :number :text (if (= :percent unit) (percent-text value) (number-text value))}
        unit (assoc :unit unit))))

(defn- cost-cell [row field priced]
  (let [value (get row field)]
    (if-not (and (true? priced) (some? value)) {:kind :null}
      (do
        (number-text value)
        (when (neg? value) (refuse! :unsupported-cost))
        (when (and (instance? java.math.BigDecimal value)
                   (not (<= -1024 (.scale ^java.math.BigDecimal value) 1024)))
          (refuse! :oversized-number))
        ;; Native notional costs may be fractional. Preserve native totals;
        ;; recurring ratios use an explicitly marked decimal presentation.
        (let [dollars (if (ratio? value)
                        (.divide (bigdec (numerator value))
                                 (.multiply (bigdec (denominator value)) 1000000M)
                                 12 RoundingMode/HALF_UP)
                        (.movePointLeft (bigdec value) 6))]
          (cond-> {:kind :number :unit :usd
                   :text (.toPlainString (.stripTrailingZeros dollars))}
            (ratio? value) (assoc :approximate? true)))))))

(defn- boolean-cell [value]
  (if (nil? value) {:kind :null}
      (do (when-not (boolean? value) (refuse! :unsupported-flag))
          {:kind :boolean :value value})))

(defn- interval-cell [value percent?]
  (if (nil? value) {:kind :null}
      (do
        (when-not (and (vector? value) (= 2 (count value))) (refuse! :unsupported-interval))
        (let [format-value (if percent? percent-text number-text)]
          {:kind :interval :text (str (format-value (first value)) "–" (format-value (second value))
                                     (when percent? "%"))}))))

(defn- cell [row kind]
  (let [spend (:spend row)]
    (case kind
      :candidate {:kind :text :text (:candidate row) :complete? true}
      :attempts (number-cell (:attempts row))
      :passed (number-cell (:passed row))
      :pass-rate (number-cell (:pass-rate row) :percent)
      :pass-rate-interval (interval-cell (:pass-rate-interval row) true)
      :reward (number-cell (:reward-mean row))
      :reward-interval (interval-cell (:reward-interval row) false)
      :cost (cost-cell spend :microdollars (:priced? spend))
      :notional-cost (cost-cell spend :notional-microdollars (:notional-priced? spend))
      :cost-per-pass (cost-cell row :microdollars-per-pass (:priced? spend))
      :notional-cost-per-pass (cost-cell row :notional-microdollars-per-pass (:notional-priced? spend))
      :cost-known (boolean-cell (:priced? spend))
      :notional-cost-known (boolean-cell (:notional-priced? spend))
      :usage-reported (boolean-cell (:usage-reported? spend)))))

(defn- pricing-info [spend]
  (when-not (map? spend) (refuse! :invalid-summary))
  (let [flags (select-keys spend [:priced? :notional-priced? :usage-reported?])]
    (when-not (every? #(or (nil? %) (boolean? %)) (vals flags)) (refuse! :invalid-summary))
    flags))

(defn- within-display-budget? [result]
  (let [text (binding [*print-length* nil *print-level* nil *print-meta* false] (pr-str result))
        encoder (doto (.newEncoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))]
    (<= (.remaining (.encode encoder (CharBuffer/wrap text))) max-presentation-bytes)))

(defn prepare
  "Project the fixed native summary, not an accounting or adoption certificate.
   Context is server-derived {:room UUID :store UUID}; no Memory ID can select it."
  [raw-descriptor context summary definition]
  (let [descriptor (spec/validate raw-descriptor)
        id (get-in descriptor [:source :scorecard]) rows (:leaderboard summary)
        candidates (:experiment/candidates definition)
        environments (get-in definition [:experiment/dataset :dataset/environments])
        repetitions (:experiment/repetitions definition)
        source (assoc context :kind :scorecard :scorecard id)
        base {:profile contract/profile :view (:id descriptor) :descriptor descriptor :source source
              :basis (hasch/uuid [:simmis/scorecard-view-v1 source]) :snapshot? true}]
    (when-not (and (= :scorecard (get-in descriptor [:source :kind]))
                   (= #{:room :store} (set (keys context))) (every? uuid? (vals context)))
      (refuse! :invalid-context))
    (when-not (and (= (str id) (:id summary))
                   (= (str (:experiment/content-id definition)) (get-in summary [:experiment :content-id]))
                   (uuid? (:experiment/content-id definition))
                   (uuid? (get-in definition [:experiment/dataset :dataset/content-id]))
                   (vector? rows) (<= 1 (count rows) contract/max-scorecard-rows)
                   (every? #(and (string? (:candidate %)) (not (str/blank? (:candidate %)))
                                  (<= (count (:candidate %)) contract/max-candidate-characters)) rows)
                   (every? #(and (integer? (:attempts %)) (<= 1 (:attempts %) 4096)
                                  (integer? (:passed %)) (<= 0 (:passed %) (:attempts %))) rows)
                   (= (count rows) (count (set (map :candidate rows))))
                   (vector? candidates) (= (count rows) (count candidates))
                   (vector? environments) (<= 1 (count environments) 128)
                   (integer? repetitions) (<= 1 repetitions 128)
                   (= (set (map (comp str :candidate/id) candidates))
                      (set (map #(str ":" (:candidate %)) rows)))
                   (integer? (:cells summary)) (<= 1 (:cells summary) 4096)
                   (= (:cells summary) (* (count candidates) (count environments) repetitions)))
      (refuse! :invalid-summary))
    (let [result
          (assoc base :status :ready :matrix-complete? true :total (count rows)
                 :complete? (<= (count rows) (:limit descriptor))
                 :display {:complete? (<= (count rows) (:limit descriptor))}
                 :matrix {:cells (:cells summary) :environments (count environments) :repetitions repetitions}
                 :provenance {:experiment-content-id (:experiment/content-id definition)
                              :dataset-content-id (get-in definition [:experiment/dataset :dataset/content-id])
                              :workflow (some-> (provenance/workflow definition) (select-keys [:name :bundle]))
                              :evaluation (some-> (provenance/evaluation definition) (select-keys [:id :version :split]))}
                 :columns (mapv #(assoc % :label (or (:label %) (get contract/scorecard-columns (:kind %))))
                                (:columns descriptor))
                 :rows (mapv (fn [row]
                               {:id [id (:candidate row)] :candidate (:candidate row) :scorecard id
                                :cells (mapv #(cell row (:kind %)) (:columns descriptor))
                                :pricing (pricing-info (:spend row))})
                             (take (:limit descriptor) rows)))]
      (if (within-display-budget? result) result
          (assoc base :status :unavailable :reason :display-limit :complete? false)))))
