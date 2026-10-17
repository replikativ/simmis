(ns is.simm.ops.benchmark-dataset
  "Versioned evaluation splits in workflow files, not another dataset store.
   Selection is enforced before provider preparation on every benchmark path."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [hasch.core :as hasch]
            [malli.core :as m])
  (:import [java.io PushbackReader StringReader]))

(def split-schema [:enum "development" "validation" "held-out"])
(def manifest-schema
  [:map {:closed true}
   [:version [:= 1]]
   [:splits [:map {:closed true}
             [:development [:vector {:min 1 :max 100} [:string {:min 1 :max 100}]]]
             [:validation {:optional true} [:vector {:min 1 :max 100} [:string {:min 1 :max 100}]]]
             [:held-out {:optional true} [:vector {:min 1 :max 100} [:string {:min 1 :max 100}]]]]]])

(defn- invalid! [message]
  (throw (ex-info message {:type :is.simm.ops.benchmarks/invalid})))

(defn manifest
  "Read optional benchmark.edn. Splits must partition all named cases exactly.
   This is shape/identity validation only; no checker or provider executes."
  [files cases]
  (when-let [text (get files "benchmark.edn")]
    (when-not (and (string? text) (<= (count text) 16384))
      (invalid! "benchmark.edn must be bounded EDN text"))
    (let [value (try (with-open [reader (PushbackReader. (StringReader. text))]
                       (let [options {:eof ::eof :readers {} :default (fn [& _] (invalid! "Unsupported dataset tag"))}
                             value (edn/read options reader)]
                         (when-not (= ::eof (edn/read options reader))
                           (invalid! "Dataset manifest must contain one EDN value"))
                         value))
                     (catch Exception _ (invalid! "Invalid benchmark.edn")))
          _ (when-not (m/validate manifest-schema value)
              (invalid! "Invalid benchmark split manifest"))
          members (vec (mapcat val (:splits value)))]
      (when-not (and (<= (count members) 100)
                     (= (count members) (count (set members)))
                     (= (set cases) (set members)))
        (invalid! "Benchmark splits must partition all named cases without duplicates"))
      (let [inputs (into (sorted-map)
                         (filter (fn [[path _]] (or (= path "gold.edn")
                                                    (str/starts-with? path "fixtures/")
                                                    (str/starts-with? path "cases/")))) files)]
        (assoc value :id (str (hasch/uuid [:simmis/benchmark-dataset-v1 value inputs])))))))

(defn select-cases
  "An explicit split is mandatory for declared datasets. Cases may narrow that
   split, never cross it. Legacy workflows retain their original selection."
  [files available {:keys [split cases]}]
  (let [dataset (manifest files available)]
    (when (and (some? split) (or (nil? dataset) (not (m/validate split-schema split))))
      (invalid! "Select a declared benchmark split"))
    (when (and dataset (nil? split))
      (invalid! "Select an explicit benchmark split; held-out cases never run implicitly"))
    (let [allowed (if dataset (get-in dataset [:splits (keyword split)]) (vec (sort available)))
          selected (or cases allowed)]
      (when-not (and (or (nil? dataset) (seq allowed))
                     (vector? selected)
                     (= (count selected) (count (set selected)))
                     (every? (set allowed) selected)
                     (or (seq selected) (and (nil? dataset) (empty? available) (nil? cases))))
        (invalid! "Benchmark cases must be a nonempty subset of the selected split"))
      {:cases selected
       :metadata (when dataset {:id (:id dataset) :version (:version dataset)
                                :split split :cases selected})})))
