(ns is.simm.documents.saved-view-spec
  "Inert bounded saved-view declarations. No query evaluation, schema, source
   access, renderer callbacks or edit authority."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [is.simm.documents.saved-view-contract :as contract])
  (:import [java.io PushbackReader StringReader]
           [java.nio CharBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]))

(def profile contract/profile)
(def max-characters contract/max-characters)
(def max-columns contract/max-columns)
(def max-rows contract/max-rows)
(def max-mounted contract/max-mounted)

(defn- refuse! [reason]
  (throw (ex-info "Saved view declaration is invalid" {:type ::refused :reason reason})))

(defn- closed! [value required optional]
  (when-not (and (map? value) (not (record? value))
                 (every? #(contains? value %) required)
                 (every? (into required optional) (keys value)))
    (refuse! :unsupported-shape)))

(defn- root? [root]
  (and (string? root) (<= (count root) 512)
       (not (re-find #"[\\\p{Cntrl}]" root))
       (or (= "" root)
           (every? #(and (not (str/blank? %))
                         (not (#{"." ".." ".git" ".geschichte"} %)))
                   (str/split root #"/" -1)))))

(defn- distinct-vector? [value minimum maximum predicate]
  (and (vector? value) (<= minimum (count value) maximum)
       (every? predicate value) (= (count value) (count (set value)))))

(defn- column! [column]
  (when-not (map? column) (refuse! :unsupported-column))
  (if (contains? column :kind)
    (do (closed! column #{:kind} #{:label})
        (when-not (= :title (:kind column)) (refuse! :unsupported-column)))
    (do (closed! column #{:property} #{:label})
        (when-not (uuid? (:property column)) (refuse! :invalid-property))))
  (when (contains? column :label)
    (when-not (and (string? (:label column)) (<= 1 (count (:label column)) 128)
                   (not (str/blank? (:label column)))
                   (not (re-find #"[\p{Cntrl}]" (:label column))))
      (refuse! :invalid-label)))
  column)

(defn- validate-pages [value]
  (closed! value #{:version :id :source :presentation :columns} #{:order :limit})
  (when-not (= 1 (:version value)) (refuse! :unsupported-version))
  (when-not (uuid? (:id value)) (refuse! :invalid-identity))
  (when-not (#{:table :form} (:presentation value)) (refuse! :unsupported-presentation))
  (let [source (:source value)
        limit (get value :limit (if (= :form (:presentation value)) 1 max-rows))
        order (get value :order [{:kind :title :direction :asc}])]
    (closed! source #{:kind :roots} #{:types :page})
    (when-not (= :pages (:kind source)) (refuse! :unsupported-source))
    (when-not (distinct-vector? (:roots source) 1 8 root?) (refuse! :invalid-roots))
    (when (contains? source :types)
      (when-not (distinct-vector? (:types source) 0 8 uuid?) (refuse! :invalid-types)))
    (when (contains? source :page)
      (when-not (uuid? (:page source)) (refuse! :invalid-page)))
    (when (and (= :form (:presentation value)) (not (uuid? (:page source))))
      (refuse! :form-needs-page))
    (when-not (and (integer? limit) (<= 1 limit max-rows)
                   (or (not= :form (:presentation value)) (= 1 limit)))
      (refuse! :invalid-limit))
    (when-not (and (vector? order) (= 1 (count order))
                   (contains? #{{:kind :title :direction :asc}
                                {:kind :title :direction :desc}} (first order)))
      (refuse! :unsupported-order))
    (when-not (and (vector? (:columns value)) (<= 1 (count (:columns value)) max-columns))
      (refuse! :invalid-columns))
    (let [columns (mapv column! (:columns value))
          identities (mapv #(select-keys % [:kind :property]) columns)]
      (when-not (= (count identities) (count (set identities))) (refuse! :duplicate-columns))
      (assoc value :source (assoc source :types (:types source [])) :columns columns
             :limit limit :order order))))

(defn- validate-scorecard [value]
  (closed! value #{:version :id :source :presentation :columns} #{:limit})
  (when-not (= 1 (:version value)) (refuse! :unsupported-version))
  (when-not (uuid? (:id value)) (refuse! :invalid-identity))
  (when-not (= :table (:presentation value)) (refuse! :unsupported-presentation))
  (let [source (:source value)
        limit (get value :limit contract/max-scorecard-rows)
        columns (:columns value)]
    (closed! source #{:kind :room :scorecard} #{})
    (when-not (and (= :scorecard (:kind source)) (= :current (:room source))
                   (uuid? (:scorecard source)))
      (refuse! :unsupported-scorecard-source))
    (when-not (and (integer? limit) (<= 1 limit contract/max-scorecard-rows))
      (refuse! :invalid-limit))
    (when-not (and (vector? columns) (<= 1 (count columns) max-columns))
      (refuse! :invalid-columns))
    (doseq [column columns]
      (closed! column #{:kind} #{:label})
      (when-not (contains? contract/scorecard-columns (:kind column))
        (refuse! :unsupported-column))
      (when (contains? column :label)
        (when-not (and (string? (:label column)) (<= 1 (count (:label column)) 128)
                       (not (str/blank? (:label column)))
                       (not (re-find #"[\p{Cntrl}]" (:label column))))
          (refuse! :invalid-label))))
    (when-not (= (count columns) (count (set (map :kind columns))))
      (refuse! :duplicate-columns))
    (assoc value :limit limit)))

(defn validate
  "Validate inert declarations for a fixed source. Neither source IDs nor
   columns carry permission, executable queries or writable field bindings."
  [value]
  (case (get-in value [:source :kind])
    :pages (validate-pages value)
    :scorecard (validate-scorecard value)
    (refuse! :unsupported-source)))

(defn- bounded-source! [source]
  (when-not (and (string? source) (<= 1 (count source) max-characters))
    (refuse! :oversized-or-empty-input))
  (try
    (let [encoder (doto (.newEncoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))]
      (when (> (.remaining (.encode encoder (CharBuffer/wrap ^String source))) max-characters)
        (refuse! :oversized-or-empty-input)))
    (catch java.nio.charset.CharacterCodingException _ (refuse! :invalid-utf8)))
  ;; Bound nesting BEFORE the EDN reader. Strings/comments cannot introduce
  ;; structure. Character literals are outside this closed descriptor grammar.
  (loop [index 0 depth 0 quoted? false escaped? false comment? false]
    (if (= index (count source))
      (when (or quoted? (not (zero? depth))) (refuse! :invalid-syntax))
      (let [c (.charAt ^String source index)
            [next-depth next-quoted? next-escaped? next-comment?]
            (cond
              comment? [depth false false (not (#{\newline \return} c))]
              quoted? (cond escaped? [depth true false false]
                            (= \\ c) [depth true true false]
                            (= \" c) [depth false false false]
                            :else [depth true false false])
              (= \; c) [depth false false true]
              (= \" c) [depth true false false]
              (= \\ c) (refuse! :unsupported-character-literal)
              (#{\{ \[ \(} c) [(inc depth) false false false]
              (#{\} \] \)} c) [(dec depth) false false false]
              :else [depth false false false])]
        (when-not (<= 0 next-depth 8) (refuse! :nesting-limit))
        (recur (inc index) next-depth next-quoted? next-escaped? next-comment?)))))

(defn decode [source]
  (bounded-source! source)
  (let [value (try
                (with-open [reader (PushbackReader. (StringReader. source))]
                  (let [eof (Object.)
                        options {:eof eof :readers {}
                                 :default (fn [& _] (refuse! :unsupported-tag))}
                        value (edn/read options reader)]
                    (when-not (identical? eof (edn/read options reader)) (refuse! :trailing-input))
                    value))
                (catch Exception _ (refuse! :invalid-syntax)))]
    (validate value)))
