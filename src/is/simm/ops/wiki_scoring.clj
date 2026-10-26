(ns is.simm.ops.wiki-scoring
  "Host observations of frozen benchmark inputs. No live room lookup or parser
   capability is installed in the checker SCI context."
  (:require [clojure.string :as str]
            [dvergr.rooms.files :as room-files]
            [is.simm.documents.wiki-facts :as facts]
            [is.simm.ops.knowledge-inputs :as inputs]))

(def profile {:id :wiki-structure-v1 :version 1 :facts facts/profile})

(def checkpoint-profile
  {:id :wiki-checkpoint-structure-v1 :version 1 :facts facts/profile
   :baseline {:source :selected-case-fixtures :root "/wiki/"}
   :evidence :verified-knowledge-manifest})

(defn selected-profile [bundle]
  (let [selected (get-in bundle [:definition :params :simmis/checker-profile])]
    (when (contains? (get-in bundle [:definition :params]) :simmis/checker-profile)
      (or (get {(:id profile) profile (:id checkpoint-profile) checkpoint-profile} selected)
          (throw (ex-info "Unknown checker observation profile" {:type ::invalid-profile}))))))

(defn- markdown? [path]
  (boolean (re-find #"(?i)\.(md|markdown)$" path)))

(defn- validate-gold! [gold guard!]
  (guard!)
  (let [text? #(and (string? %) (<= (count %) 65536))
        fragments? #(and (vector? %) (<= (count %) 100) (every? text? %))
        expected #{:version :no-change? :required :preserved :forbidden :citations :quotes}]
    (when-not (and (map? gold) (= 2 (:version gold)) (boolean? (:no-change? gold))
                   (every? #(contains? gold %) expected)
                   (every? (conj expected :sources) (keys gold))
                   (every? fragments? ((juxt :required :forbidden :citations) gold))
                   (map? (:preserved gold)) (<= (count (:preserved gold)) 100)
                   (every? (fn [[path fragments]]
                             (guard!)
                             (and (string? path) (<= 1 (count path) 512) (fragments? fragments))) (:preserved gold))
                   (vector? (:quotes gold)) (<= (count (:quotes gold)) 100)
                   (every? (fn [quote]
                             (guard!)
                             (and (map? quote) (= #{:source :text} (set (keys quote)))
                                  (text? (:source quote)) (text? (:text quote)))) (:quotes gold))
                   (or (not (contains? gold :sources))
                       (and (map? (:sources gold)) (<= (count (:sources gold)) 100)
                            (every? (fn [[path text]] (and (text? path) (text? text))) (:sources gold)))))
      (throw (ex-info "Wiki structural rubric requires explicit bounded V2 constraints" {:type ::invalid-gold}))))
  gold)

(defn- checkpoint-baseline [baseline fixtures guard!]
  ;; Candidate-visible wiki context is useful for preservation checks, but it
  ;; is not new source evidence. Never add it to quote/citation source texts.
  (reduce-kv
   (fn [result path text]
     (guard!)
     (if (and (string? path) (str/starts-with? path "/wiki/") (markdown? path))
       (do
         (when-not (and (string? text)
                        (try (= path (room-files/workspace-path path)) (catch Exception _ false)))
           (throw (ex-info "Checkpoint wiki fixture has an invalid path or body" {:type ::invalid-baseline})))
         (let [relative (subs path 1)]
           (when (and (contains? result relative) (not= (get result relative) text))
             (throw (ex-info "Checkpoint and copied wiki fixtures disagree" {:type ::conflicting-baseline})))
           (assoc result relative text)))
       result))
   baseline fixtures))

(defn prepare-input
  "Called inside the scoring account. Select precisely the candidate's shared
   and case fixtures, verify their frozen manifest, then observe plain facts."
  [bundle selection-policy environment guard! input]
  (guard!)
  (when-not selection-policy
    (throw (ex-info "Wiki structural scoring requires explicit knowledge inputs" {:type ::missing-policy})))
  (let [observation-profile (or (selected-profile bundle) profile)
        case-id (get-in environment [:environment/metadata :case])
        cases (:cases bundle)
        _ (when-not (if (seq cases) (contains? cases case-id) (nil? case-id))
            (throw (ex-info "Wiki scoring case is not in the pinned bundle" {:type ::unknown-case})))
        gold (validate-gold! (if (seq cases) (get-in cases [case-id :gold]) (:gold bundle)) guard!)
        fixtures (merge (:fixtures bundle) (get-in cases [case-id :fixtures]))
        _ (guard!)
        captured (inputs/from-inputs selection-policy fixtures)
        _ (guard!)
        repository (get-in captured [:manifest :repository])
        entries (get-in captured [:manifest :files])
        evidence-entries (if (= (:id checkpoint-profile) (:id observation-profile))
                           (filterv #(= :trigger (:role %)) entries)
                           entries)
        baseline (into (sorted-map)
                       (for [{:keys [path input]} entries :when (markdown? path)]
                         (do (guard!) [path (get fixtures input)])))
        baseline (if (= (:id checkpoint-profile) (:id observation-profile))
                   (checkpoint-baseline baseline fixtures guard!)
                   baseline)
        proposed (into (sorted-map)
                       (for [[path text] (:files input)
                             :when (and (string? path) (str/starts-with? path "/out/wiki/")
                                        (markdown? path) (string? text))]
                         (do (guard!) [(subs path 5) text])))
        baseline-facts (facts/documents repository baseline guard!)
        _ (when-not (every? :valid? (vals (:files baseline-facts)))
            (throw (ex-info "Frozen wiki fixture has invalid structure" {:type ::invalid-baseline})))
        ids (mapcat :ids (vals (:files baseline-facts)))
        _ (when-not (= (count ids) (count (set ids)))
            (throw (ex-info "Frozen wiki fixture has duplicate identities" {:type ::invalid-baseline})))]
    (assoc input :gold gold :structure
           {:profile observation-profile
            :source-paths (mapv :input evidence-entries)
            :source-texts (into (sorted-map) (map (fn [{:keys [input]}] (guard!) [input (get fixtures input)])) evidence-entries)
            :baseline-texts baseline
            :baseline baseline-facts
            :proposed (facts/documents repository proposed guard!)})))
