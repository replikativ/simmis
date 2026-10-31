(ns is.simm.model.room-summaries
  "Contextual summary contracts. Native revisions are authoritative; documents
   and chat are projections. Coverage is independent of wiki incorporation."
  (:require [hasch.core :as hasch]
            [is.simm.model.conversation-digest :as digest]
            [malli.core :as m]))

(def max-originals 64)
(def max-context 4)
(def max-input-bytes (* 512 1024))
(def manifest-path "/in/summary/manifest.edn")
(def output-path "/out/summary.edn")

(def period-schema
  [:map {:closed true}
   [:kind [:enum :day :week :month :year :all-time]]
   [:timezone [:string {:min 1 :max 100}]]
   [:from [:fn inst?]] [:until [:fn inst?]]])

(def source-schema
  [:map {:closed true}
   [:source-id :uuid] [:source-revision :uuid]
   [:path [:fn digest/source-path?]]
   [:file-content :uuid]
   [:kind [:enum :conversation :capture :document :summary]]
   [:authority [:enum :human :assistant :tool :capture :unknown]]
   [:through {:optional true} [:fn inst?]]])

(def selection-schema
  [:map {:closed true}
   [:version [:= 2]] [:id :uuid] [:room :uuid] [:store :uuid]
   [:period period-schema] [:cutoff [:fn inst?]]
   ;; A cutoff inventories current revisions, not unavailable deleted history.
   [:inventory [:enum :current-revisions]]
   [:complete? :boolean]
   [:coverage [:vector {:max max-originals} source-schema]]
   [:context [:vector {:max max-context} source-schema]]])

(defn- refuse! [reason]
  (throw (ex-info "Invalid contextual room summary"
                  {:type ::invalid :reason reason})))

(defn before? [a b] (< (.getTime a) (.getTime b)))
(defn- ordered [sources]
  (vec (sort-by (juxt :path (comp str :source-id) (comp str :source-revision)) sources)))

(defn node-id
  "Stable logical node. Different workflow/model executions create revisions,
   not different identities for the same room interval."
  [{:keys [room store period]}]
  (when-not (and (uuid? room) (uuid? store) (m/validate period-schema period)
                 (before? (:from period) (:until period)))
    (refuse! :invalid-period))
  (hasch/uuid [:simmis/room-summary-node-v1 room store period]))

(defn validate-selection! [value]
  (when-not (m/validate selection-schema value) (refuse! :invalid-selection))
  (node-id value)
  (let [sources (into (:coverage value) (:context value))
        revisions (mapv :source-revision sources)]
    (when-not (and (= (:coverage value) (ordered (:coverage value)))
                   (= (:context value) (ordered (:context value)))
                   (= (count revisions) (count (set revisions)))
                   (= (:id value) (hasch/uuid [:simmis/room-summary-selection-v2 (dissoc value :id)]))
                   (not (before? (:cutoff value) (get-in value [:period :until])))
                   (every? (fn [source]
                             (or (not= :summary (:kind source))
                                 (and (inst? (:through source))
                                      (not (before? (get-in value [:period :from]) (:through source))))))
                           (:context value)))
      (refuse! :invalid-selection)))
  value)

(defn selection [value]
  (let [body (-> value (dissoc :id) (assoc :version 2)
                 (update :coverage ordered) (update :context ordered))]
    (validate-selection! (assoc body :id (hasch/uuid [:simmis/room-summary-selection-v2 body])))))

(def report-schema
  [:map {:closed true} [:version [:= 2]] [:selection-id :uuid]
   [:summary digest/item-schema]
   [:coverage
    [:vector {:max max-originals}
     [:map {:closed true} [:source-revision :uuid]
      [:status [:enum :selected :omitted :deferred]] [:note [:fn digest/plain-text?]]]]]
   [:sections
    (into [:map {:closed true}]
          (map (fn [key] [key [:vector {:max 20} digest/item-schema]]) digest/section-keys))]])

(defn validate-report!
  "Context may support a citation, never a fresh coverage disposition. A valid
   structural report is not proof of truth or complete semantic understanding."
  [selection report]
  (validate-selection! selection)
  (when-not (and (m/validate report-schema report)
                 (= (:id selection) (:selection-id report))
                 (<= (digest/utf8-size (pr-str report)) digest/max-digest-bytes))
    (refuse! :invalid-report))
  (let [coverage (:coverage report)
        reported (mapv :source-revision coverage)
        originals (set (map :source-revision (:coverage selection)))
        context (set (map :source-revision (:context selection)))
        selected (into #{} (keep #(when (= :selected (:status %)) (:source-revision %))) coverage)
        items (cons (:summary report) (mapcat #(get-in report [:sections %]) digest/section-keys))]
    (when-not (and (= originals (set reported)) (= (count originals) (count reported)))
      (refuse! :invalid-coverage))
    (doseq [[index item] (map-indexed vector items)]
      (when-not (and (or (seq (:sources item))
                         ;; An entirely deferred result can explain its hold,
                         ;; but cannot fabricate a supporting source citation.
                         (and (zero? index) (empty? selected) (empty? context)))
                     (= (count (:sources item)) (count (set (:sources item))))
                     (every? #(or (contains? selected %) (contains? context %)) (:sources item)))
        (refuse! :invalid-citation))))
  report)

(defn ready?
  "Empty/partial/deferred results never become a complete period. Context
   alone does not establish activity or advance original coverage."
  [selection report]
  (validate-report! selection report)
  (boolean (and (:complete? selection) (seq (:coverage selection))
                (not-any? #(= :deferred (:status %)) (:coverage report)))))

(defn revision-id [selection report program]
  (validate-report! selection report)
  (when-not (and (uuid? (:bundle program)) (string? (:model program))
                 (uuid? (:request program)) (uuid? (:attempt program)))
    (refuse! :invalid-program))
  (hasch/uuid [:simmis/room-summary-revision-v1
              (node-id selection) selection report program]))
