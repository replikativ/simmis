(ns is.simm.model.conversation-digest
  "Portable optional navigation digest. Integrity and attribution only: a digest
   never acknowledges original evidence, changes eligibility or grants authority."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [malli.core :as m]))

(def max-sources 8)
(def max-selection-characters 4000)
(def max-digest-bytes 65536)
(def section-keys [:decisions :proposals :observations :uncertainty :open-work])
(def authorities #{:human :assistant :tool :capture :unknown})

(defn- refuse! [reason]
  (throw (ex-info "Invalid conversation digest" {:type ::invalid :reason reason})))

(defn- path? [path]
  (and (string? path) (<= 1 (count path) 512)
       (not (re-find #"[\\\u0000-\u001f\u007f]" path))
       (every? #(and (not (str/blank? %)) (not (#{"." ".." ".git" ".geschichte"} %)))
               (str/split path #"/" -1))))

(defn- text? [text]
  (and (string? text) (<= 1 (count text) 4000) (not (str/blank? text))
       ;; Output is plain text plus structured source references, never HTML.
       (not (re-find #"<(?:/?[a-z][a-z0-9-]*(?:\s|/?>)|!doctype|!--)" (str/lower-case text)))))

(def source-schema
  [:map {:closed true} [:source-id :uuid] [:source-revision :uuid]
   [:path [:fn path?]] [:authority [:enum :human :assistant :tool :capture :unknown]]
   [:source-kind [:enum :conversation :capture :document]]
   [:message-id {:optional true} :uuid]
   [:content-id {:optional true} :uuid] [:file-content {:optional true} :uuid]])

(def selection-schema
  [:map {:closed true} [:version [:= 1]] [:id :uuid]
   [:sources [:vector {:min 1 :max max-sources} source-schema]]])

(defn- ordered [sources]
  (vec (sort-by (juxt :path (comp str :source-id) (comp str :source-revision)) sources)))

(defn validate-selection!
  "Checks exact host metadata identity, not provenance or retained file bytes."
  [selection]
  (when-not (and (m/validate selection-schema selection)
                 (<= (count (pr-str selection)) max-selection-characters)
                 (= (:sources selection) (ordered (:sources selection)))
                 (every? (fn [key]
                           (= (count (:sources selection)) (count (set (map key (:sources selection))))))
                         [:source-id :source-revision :path])
                 (= (:id selection)
                    (hasch/uuid [:simmis/conversation-digest-selection-v1 (dissoc selection :id)])))
    (refuse! :invalid-selection))
  selection)

(defn selection
  "Seal bounded explicit original refs. Room-evidence artifacts supply message-id,
   source-revision, content-id and path. Other originals supply source-id instead.
   HOST supplies authority/kind and verifies retained text before admission;
   file-content, when supplied, is the exact retained file's content identity."
  [artifacts]
  (when-not (and (vector? artifacts) (<= 1 (count artifacts) max-sources))
    (refuse! :selection-too-large))
  (let [sources (ordered
                 (mapv (fn [artifact]
                         (merge (select-keys artifact [:message-id :content-id :file-content])
                                {:source-id (or (:source-id artifact) (:message-id artifact))
                                 :source-revision (:source-revision artifact) :path (:path artifact)
                                 :authority (or (:authority artifact) :unknown)
                                 :source-kind (or (:source-kind artifact)
                                                  (if (:message-id artifact) :conversation :document))})) artifacts))
        body {:version 1 :sources sources}]
    (validate-selection! (assoc body :id (hasch/uuid [:simmis/conversation-digest-selection-v1 body])))))

(def item-schema
  [:map {:closed true} [:text [:fn text?]]
   [:sources [:vector {:max max-sources} :uuid]]])

(def digest-schema
  [:map {:closed true} [:version [:= 1]] [:selection-id :uuid]
   [:summary item-schema]
   [:coverage [:vector {:min 1 :max max-sources}
               [:map {:closed true} [:source-revision :uuid]
                [:status [:enum :selected :omitted :deferred]] [:note [:fn text?]]]]]
   [:sections
    (into [:map {:closed true}]
          (map (fn [key] [key [:vector {:max 20} item-schema]]) section-keys))]])

(defn- byte-count [text]
  #?(:clj (alength (.getBytes ^String text "UTF-8"))
     :cljs (.-length (.encode (js/TextEncoder.) text))))

(defn validate!
  "Verify exact admitted membership and structured citations. OMITTED means read
   but not represented; DEFERRED means unresolved/uninspected. Neither affects
   wiki eligibility. These model claims do not prove understanding or truth."
  [selection digest]
  (validate-selection! selection)
  (when-not (and (m/validate digest-schema digest)
                 (= (:id selection) (:selection-id digest))
                 (<= (byte-count (pr-str digest)) max-digest-bytes))
    (refuse! :invalid-digest))
  (let [expected (set (map :source-revision (:sources selection)))
        coverage (:coverage digest)
        reported (mapv :source-revision coverage)
        selected (into #{} (keep #(when (= :selected (:status %)) (:source-revision %))) coverage)
        items (cons (:summary digest) (mapcat #(get-in digest [:sections %]) section-keys))]
    (when-not (and (= (count reported) (count (set reported))) (= expected (set reported)))
      (refuse! :invalid-membership))
    (doseq [{:keys [sources]} items]
      (when-not (and (= (count sources) (count (set sources))) (every? selected sources))
        (refuse! :invalid-citation)))
    (when (some #(empty? (:sources %)) (mapcat #(get-in digest [:sections %]) section-keys))
      (refuse! :uncited-item)))
  digest)

(defn digest-id [selection digest]
  (validate! selection digest)
  (hasch/uuid [:simmis/conversation-digest-v1 digest]))
