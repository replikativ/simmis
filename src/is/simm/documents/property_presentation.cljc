(ns is.simm.documents.property-presentation
  "Pure property read-model adaptation to shared typed display cells.
   No input parsing, source acquisition, inferred permissions or writes.")

(def max-preview-characters 1024)

(defn- code-at [text index]
  #?(:clj (int (.charAt ^String text index)) :cljs (.charCodeAt text index)))

(defn- text-cell [kind text]
  (if (string? text)
    (let [end (min (count text) max-preview-characters)
          end (if (and (pos? end) (< end (count text))
                       (<= 0xD800 (code-at text (dec end)) 0xDBFF)
                       (<= 0xDC00 (code-at text end) 0xDFFF)) (dec end) end)]
      {:kind kind :text (subs text 0 end) :complete? (= end (count text))})
    {:kind :unavailable-value}))

(defn- reference-cell [model field]
  (let [text (get-in field [:input :text])
        id (or (get-in field [:relation :target])
               (when (and (string? text) (= 36 (count text))) (parse-uuid text)))
        options (get-in model [:reference-options (get-in field [:relation :codomain])])
        item (when (and (uuid? id) (= :ready (:status options)))
               (some #(when (= (str id) (:id %)) %) (:items options)))]
    (cond
      (not (uuid? id)) {:kind :unavailable-value}
      (and item (string? (:label item)))
      (cond-> {:kind :reference :id id :label (:label item)}
        (string? (:path item)) (assoc :path (:path item)))
      :else {:kind :unresolved-reference :id id
             :reason (cond (not= :ready (:status options)) :lookup-unavailable
                           (false? (:complete? options)) :lookup-incomplete
                           :else :not-resolved)})))

(defn cell
  "Use authoritative scalar input text and storage type, never a rendering
   hint to reinterpret values. Keep missing/null/false/zero/empty distinct.
   Unsupported structures remain inert source previews; they are not decoded."
  [model field]
  (let [input (:input field) text (:text input)]
    (cond
      (false? (:present? field)) {:kind :missing}
      (true? (:null input)) {:kind :null}
      (not= :one (:cardinality field)) (text-cell :source-value text)
      (= :db.type/ref (:value-type field)) (reference-cell model field)
      (= :db.type/boolean (:value-type field))
      (case text "true" {:kind :boolean :value true}
                 "false" {:kind :boolean :value false}
                 {:kind :unavailable-value})
      (contains? #{:db.type/long :db.type/double} (:value-type field))
      (if (and (string? text) (<= (count text) max-preview-characters))
        {:kind :number :text text} {:kind :unavailable-value})
      (= :db.type/string (:value-type field)) (text-cell :text text)
      (= :db.type/uuid (:value-type field)) (text-cell :uuid text)
      (= :db.type/instant (:value-type field)) (text-cell :instant text)
      :else (text-cell :source-value text))))
