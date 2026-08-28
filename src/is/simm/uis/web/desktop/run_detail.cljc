(ns is.simm.uis.web.desktop.run-detail
  "Reactive, presentation-sized projection of one causal Dvergr Run.

   The room database remains authoritative. This namespace does not mirror Run
   state: it joins durable Run identity, correlated messages, and typed tool
  calls at read time, then performs deterministic display-only compression."
  (:require [clojure.string :as str]
            [datahike.api :as d]))

(defn- uuid-value [x]
  (cond
    (uuid? x) x
    (nil? x) nil
    :else
    (try
      #?(:clj (java.util.UUID/fromString (str x))
         :cljs (uuid (str x)))
      (catch #?(:clj Throwable :cljs :default) _ nil))))

(defn tool-family
  "Semantic family used for compact labels. The durable record keeps the exact
   tool name; this is deliberately only a display projection."
  [tool-name]
  (let [n (-> (or tool-name "tool") str/lower-case)]
    (cond
      (re-find #"(read|search|query|find|list|look|view|fetch|get|slurp)" n) :read
      (re-find #"(write|edit|patch|create|add|update|delete|remove|transact)" n) :write
      (re-find #"(shell|bash|command|exec|clojure_eval)" n) :execute
      (re-find #"(browser|chrome|screen|image)" n) :observe
      (re-find #"(agent|spawn|collabor|message|notify)" n) :coordinate
      :else :generic)))

(defn- family-label [family count]
  (let [noun (if (= count 1) "tool call" "tool calls")]
    (case family
      :read (str "Read from " count " " (if (= count 1) "source" "sources"))
      :write (str "Changed " count " " (if (= count 1) "item" "items"))
      :execute (str "Ran " count " " (if (= count 1) "command" "commands"))
      :observe (str "Inspected " count " " (if (= count 1) "view" "views"))
      :coordinate (str "Coordinated " count " " (if (= count 1) "action" "actions"))
      (str "Ran " count " " noun))))

(defn- grouping-eligible? [call]
  (and (not (:error? call))
       (not= :error (:status call))
       (not= :pending-approval (:approval call))
       (not= :rejected (:approval call))))

(defn- same-kind-pass [calls]
  (loop [remaining (seq calls)
         out []]
    (if-let [call (first remaining)]
      (if-not (grouping-eligible? call)
        (recur (next remaining) (conj out {:kind :call :call call}))
        (let [family (tool-family (:name call))
              tool-name (:name call)
              run (take-while #(and (grouping-eligible? %)
                                    (= tool-name (:name %)))
                              remaining)
              n (count run)
              minimum (if (= family :write) 2 3)]
          (if (>= n minimum)
            (recur (drop n remaining)
                   (conj out {:kind :summary
                              :variant :same-kind
                              :family family
                              :label (family-label family n)
                              :calls (vec run)}))
            (recur (drop n remaining)
                   (into out (map #(hash-map :kind :call :call %) run))))))
      out)))

(defn- burst-participant? [segment]
  (or (and (= :call (:kind segment))
           (grouping-eligible? (:call segment)))
      (and (= :summary (:kind segment))
           (= :same-kind (:variant segment)))))

(defn- segment-calls [segment]
  (if (= :call (:kind segment)) [(:call segment)] (:calls segment)))

(defn- mixed-pass [segments]
  (loop [remaining (seq segments)
         out []]
    (if-let [segment (first remaining)]
      (if-not (burst-participant? segment)
        (recur (next remaining) (conj out segment))
        (let [burst (take-while burst-participant? remaining)
              calls (vec (mapcat segment-calls burst))]
          (if (>= (count burst) 2)
            (recur (drop (count burst) remaining)
                   (conj out {:kind :summary
                              :variant :mixed
                              :family :mixed
                              :label (str "Ran " (count calls) " tool calls")
                              :calls calls}))
            (recur (next remaining) (conj out segment)))))
      out)))

(defn group-tool-activity
  "Compress routine tool work in two passes while keeping failures and approval
   boundaries explicit. Every source call remains available under :calls."
  [calls]
  (-> calls same-kind-pass mixed-pass vec))

(defn- run-sort-key [run]
  [(or (:started-at run) (:created-at run) 0) (str (:id run))])

(defn causal-forest
  "Project a bounded collection of Run summaries into explicit containment.

   Chronology only orders siblings; :parent-id supplies structure. Orphans are
   retained as roots, and malformed cycles are broken deterministically so no
   Run disappears from the projection."
  [runs]
  (let [ordered (->> runs
                     (remove (comp nil? :id))
                     (sort-by run-sort-key #(compare %2 %1))
                     vec)
        by-id (into {} (map (juxt (comp str :id) identity)) ordered)
        ids (set (keys by-id))
        children-by-parent
        (->> ordered
             (keep (fn [run]
                     (let [id (str (:id run))
                           parent-id (some-> (:parent-id run) str)]
                       (when (and parent-id (contains? ids parent-id)
                                  (not= id parent-id))
                         [parent-id id]))))
             (reduce (fn [m [parent-id child-id]]
                       (update m parent-id (fnil conj []) child-id))
                     {}))
        roots (->> ordered
                   (filter (fn [run]
                             (let [id (str (:id run))
                                   parent-id (some-> (:parent-id run) str)]
                               (or (nil? parent-id)
                                   (not (contains? ids parent-id))
                                   (= id parent-id)))))
                   (mapv (comp str :id)))
        candidate-ids (concat roots (map (comp str :id) ordered))]
    (letfn [(walk [id seen path]
              (if (or (contains? seen id) (contains? path id))
                [nil seen]
                (let [run (get by-id id)
                      [children seen']
                      (reduce
                       (fn [[nodes visited] child-id]
                         (let [[node visited'] (walk child-id visited (conj path id))]
                           [(cond-> nodes node (conj node)) visited']))
                       [[] (conj seen id)]
                       (get children-by-parent id []))]
                  [{:run run
                    :children children
                    :parent-missing? (let [parent-id (some-> (:parent-id run) str)]
                                       (and parent-id (not (contains? ids parent-id))))}
                   seen'])))]
      (first
       (reduce
        (fn [[nodes seen] id]
          (let [[node seen'] (walk id seen #{})]
            [(cond-> nodes node (conj node)) seen']))
        [[] #{}]
        candidate-ids)))))

(do
     (def ^:private run-pull
       '[:run/id :run/kind :run/actor :run/trigger :run/parent :run/status
         :run/created-at :run/started-at :run/updated-at :run/ended-at
         :run/reason :run/error])

     (def ^:private message-pull
       '[:message/id :message/content :message/created-at :message/role
         :message/from :message/to :message/in-reply-to :message/thread-root-id
         :message/run-id :message/reasoning :message/source-user
         :message/source-username])

     (def ^:private tool-call-pull
       '[:tool-call/id :tool-call/name :tool-call/input :tool-call/result
         :tool-call/duration-ms :tool-call/error? :tool-call/status
         :tool-call/approval :tool-call/tool-use-id :tool-call/run-id
         :tool-call/started-at])

     (defn- actor-party-id [actor]
       (when (and (keyword? actor)
                  (or (= "party" (namespace actor)) (nil? (namespace actor))))
         (uuid-value (name actor))))

     (defn- party-names [db]
       (into {}
             (d/q '[:find ?id ?name
                    :where
                    [?p :entity/uuid ?id]
                    [?p :S.User/display-name ?name]]
                  db)))

     (defn- normalize-run [run names]
       (when run
         (let [actor (:run/actor run)]
           (cond-> {:id (str (:run/id run))
                    :kind (:run/kind run)
                    :actor actor
                    :actor-name (or (get names (actor-party-id actor))
                                    (some-> actor name))
                    :trigger-id (str (:run/trigger run))
                    :status (:run/status run)
                    :created-at (some-> (:run/created-at run) .getTime)
                    :started-at (some-> (:run/started-at run) .getTime)
                    :updated-at (some-> (:run/updated-at run) .getTime)}
             (:run/parent run) (assoc :parent-id (str (:run/parent run)))
             (:run/ended-at run) (assoc :ended-at (.getTime (:run/ended-at run)))
             (:run/reason run) (assoc :reason (:run/reason run))
             (:run/error run) (assoc :error (:run/error run))))))

     (defn- normalize-message [m names]
       (when m
         (let [from (:message/from m)
               party-id (actor-party-id from)]
           {:id (str (:message/id m))
            :content (:message/content m)
            :role (:message/role m)
            :reasoning (:message/reasoning m)
            :from from
            :author-name (or (get names party-id)
                             (:message/source-username m)
                             (:message/source-user m)
                             (some-> from name))
            :sent-at (some-> (:message/created-at m) .getTime)
            :in-reply-to (some-> (:message/in-reply-to m) str)
            :thread-root-id (some-> (:message/thread-root-id m) str)
            :run-id (some-> (:message/run-id m) str)})))

     (defn- normalize-tool-call [call]
       {:id (str (:tool-call/id call))
        :name (:tool-call/name call)
        :input (:tool-call/input call)
        :result (:tool-call/result call)
        :duration-ms (:tool-call/duration-ms call)
        :error? (:tool-call/error? call)
        :status (:tool-call/status call)
        :approval (:tool-call/approval call)
        :tool-use-id (:tool-call/tool-use-id call)
        :started-at (some-> (:tool-call/started-at call) .getTime)})

     (defn query-run-detail
       "Bounded indexed lookup for one Run and its causal projection. Returns nil
        until the room replica contains the Run."
       [db run-id]
       (when-let [run-id (uuid-value run-id)]
         (let [names (party-names db)
               run-eid (d/q '[:find ?r . :in $ ?id :where [?r :run/id ?id]]
                            db run-id)]
           (when run-eid
             (let [run-entity (d/pull db run-pull run-eid)
                   trigger-id (:run/trigger run-entity)
                   parent-id (:run/parent run-entity)
                   parent-eid (when parent-id
                                (d/q '[:find ?r . :in $ ?id
                                       :where [?r :run/id ?id]]
                                     db parent-id))
                   trigger-eid (d/q '[:find ?m . :in $ ?id
                                      :where [?m :message/id ?id]]
                                    db trigger-id)
                   message-eids (d/q '[:find [?m ...] :in $ ?id
                                       :where [?m :message/run-id ?id]]
                                     db run-id)
                   tool-eids (d/q '[:find [?t ...] :in $ ?id
                                    :where [?t :tool-call/run-id ?id]]
                                  db run-id)
                   child-eids (d/q '[:find [?r ...] :in $ ?id
                                     :where [?r :run/parent ?id]]
                                   db run-id)]
               {:run (normalize-run run-entity names)
                :trigger (some->> trigger-eid (d/pull db message-pull)
                                  (#(normalize-message % names)))
                :parent (some->> parent-eid (d/pull db run-pull)
                                 (#(normalize-run % names)))
                :messages (->> message-eids
                               (map #(normalize-message (d/pull db message-pull %) names))
                               (sort-by (juxt :sent-at :id))
                               vec)
                :tool-calls (->> tool-eids
                                 (map #(normalize-tool-call (d/pull db tool-call-pull %)))
                                 (sort-by (juxt :started-at :id))
                                 vec)
                :children (->> child-eids
                               (map #(normalize-run (d/pull db run-pull %) names))
                               (sort-by (juxt :started-at :id))
                               vec)}))))))
