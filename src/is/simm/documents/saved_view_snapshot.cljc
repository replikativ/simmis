(ns is.simm.documents.saved-view-snapshot
  "Explicit portable display snapshots. Pure formatting, no reads or writes.
   Recorded identities are provenance, never a new certificate or capability."
  (:require [clojure.string :as str]
            [is.simm.documents.links :as links]
            [is.simm.documents.saved-view-contract :as contract]
            [is.simm.documents.saved-view-values :as values]
            #?(:clj [cheshire.core :as json])))

(def max-characters 262144)

(defn- refuse! []
  (throw (ex-info "This view cannot be exported within the snapshot limits."
                  {:type ::refused})))

(defn- bounded! [value depth budget characters]
  (when (or (> depth 12) (neg? (vswap! budget dec))) (refuse!))
  (cond
    (string? value) (when (or (> (count value) 16384)
                             (neg? (vswap! characters - (count value)))) (refuse!))
    (map? value) (do (when (> (count value) 64) (refuse!))
                    (doseq [[k v] value] (bounded! k (inc depth) budget characters) (bounded! v (inc depth) budget characters)))
    (vector? value) (do (when (> (count value) 128) (refuse!))
                       (doseq [v value] (bounded! v (inc depth) budget characters)))
    (or (nil? value) (keyword? value) (uuid? value) (boolean? value) (number? value)) nil
    :else (refuse!)))

(defn- wire [value]
  (cond
    (uuid? value) (str value)
    (keyword? value) (subs (str value) 1)
    (map? value) (into {} (map (fn [[k v]] [(wire k) (wire v)])) value)
    (vector? value) (mapv wire value)
    :else value))

(defn- json [value]
  #?(:clj (json/generate-string (wire value))
     :cljs (js/JSON.stringify (clj->js (wire value)))))

(defn- text [value]
  (when-not (string? value) (refuse!))
  (-> value
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace #"[\\`*_{}\[\]()#!|~$]" #(str "\\" %))
      (str/replace "\r" "&#13;") (str/replace "\n" "&#10;")))

(defn- linked [label href]
  (if href (str "[" (text label) "](" href ")") (text label)))

(defn- cell [value context]
  (case (:kind value)
    (:number :interval :text :boolean :null :truncated) (text (values/scalar-text value))
    (:uuid :instant) (text (:text value))
    :page (linked (:text value) (links/route {:kind :memory-file :room (:room context) :path (:path value)}))
    :reference (linked (or (:label value) (some-> value :id str) "Reference")
                       (when (:path value) (links/route {:kind :memory-file :room (:room context) :path (:path value)})))
    :media (let [href (links/media-route (merge (select-keys context [:room :target])
                                              (select-keys value [:path :content])))]
             (when-not href (refuse!))
             (linked (:text value) href))
    :list (str (str/join ", " (map #(cell % context) (:items value)))
               (when (false? (:complete? value)) "…"))
    :missing "Not provided"
    :unsupported "Source value"
    :unresolved-reference "Unresolved reference"
    (refuse!)))

(defn- row-provenance [row]
  (assoc (select-keys row [:id :path :candidate :scorecard :summary])
         :media (vec (mapcat (fn [[index value]]
                              (for [cell (tree-seq #(= :list (:kind %)) :items value)
                                    :when (= :media (:kind cell))]
                                (assoc (select-keys cell [:path :content :size]) :column index)))
                            (map-indexed vector (:cells row))))))

(defn markdown
  "Format ONE ready reply accepted by the caller's current-source guard.
   Context has host-selected room/target and caller-generated id/time.
   Does not fetch media, evaluate a declaration or claim output is complete."
  [reply {:keys [id room target created-at] :as context}]
  (bounded! reply 0 (volatile! 20000) (volatile! max-characters))
  (when-not (and (= contract/profile (:profile reply)) (= :ready (:status reply))
                 (every? uuid? [id room target (:view reply) (:basis reply) (:revision reply)])
                 (links/path? (:path reply)) (uuid? (get-in reply [:expected :content]))
                 (= target (:target-stamp reply)) (string? created-at) (<= (count created-at) 64)
                 (vector? (:columns reply)) (<= 1 (count (:columns reply)) contract/max-columns)
                 (vector? (:rows reply)) (<= (count (:rows reply)) contract/max-rows)
                 (integer? (:total reply)) (<= (count (:rows reply)) (:total reply))
                 (every? #(and (vector? (:cells %)) (= (count (:columns reply)) (count (:cells %)))) (:rows reply)))
    (refuse!))
  (let [kind (get-in reply [:descriptor :source :kind])
        title (case kind :pages "Page collection snapshot" :scorecard "Benchmark results snapshot"
                         :archived-captures "Screen observations snapshot"
                         :room-summaries "Conversation summaries snapshot" (refuse!))
        metadata {"simmis-id" id
                  "simmis-snapshot" (merge {:version 1 :created-at created-at :room room :target target}
                                           (select-keys reply [:view :basis :path :expected :revision :from :to :descriptor
                                                               :source :selection :complete? :display :total :matrix-complete?
                                                               :matrix :provenance])
                                           {:columns (mapv #(select-keys % [:kind :property :label :label-complete?]) (:columns reply))
                                            :rows (mapv row-provenance (:rows reply))})}
        row #(str "| " (str/join " | " %) " |\n")
        output (str "---\n" (json metadata) "\n---\n# " title "\n\n"
                    "Display snapshot · " (count (:rows reply)) " of " (:total reply) " rows.\n\n"
                    "Source selection complete: " (case (:complete? reply) true "yes" false "no" "not reported")
                    ". Display previews complete: " (case (get-in reply [:display :complete?]) true "yes" false "no" "not reported") ".\n\n"
                    (row (map #(text (:label %)) (:columns reply)))
                    (row (repeat (count (:columns reply)) "---"))
                    (apply str (map #(row (map (fn [v] (cell v context)) (:cells %))) (:rows reply)))
                    "\nThis is a saved display, not an execution or adoption receipt. Source identities and display limits are recorded in its document properties.\n"
                    (case kind
                      :scorecard "\nModel cost uses reported usage, not wallet or grant totals. A dash means unknown.\n"
                      :archived-captures "\nMedia bytes are not embedded. Links are application-relative and authenticated. Original recording links require access to the source room and recorded file content; alignment is estimated.\n"
                      ""))]
    (when (> (count output) max-characters) (refuse!))
    output))
