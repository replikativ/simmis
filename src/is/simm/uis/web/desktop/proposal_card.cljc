(ns is.simm.uis.web.desktop.proposal-card
  (:require [is.simm.uis.web.desktop.captured-result :as captured]))

(def proposal-state-key
  "Presentation field embedded in an interval item so keyed rendering observes
   canonical Proposal changes even when the durable message is unchanged."
  ::state)

(defn attach-state
  "Attach Proposal presentation state to a proposal-message timeline item.
   Non-proposal rows are returned unchanged."
  [item state]
  (if (= :proposal (get-in item [:message/metadata :object :kind]))
    (assoc item proposal-state-key state)
    item))

(defn verification-outcome
  "Stable result of one visibility-safe proposal status request."
  [status error]
  (if error
    {:status :error :error error}
    {:status (case status
               :accepted :accepted
               :dismissed :dismissed
               :open :open
               :unavailable)}))

(defn outcome->state
  "Turn a cached status outcome into the compact card contract."
  [{:keys [status error]}]
  (case status
    :error {:status :error :proposal {:error error}}
    :open {:status :checking
           :proposal {:action-error "Proposal is open, but has not reached this list yet."
                      :retry? true}}
    (:accepted :dismissed :unavailable) {:status status}
    {:status :checking}))

(defn begin-diff-request
  "Mark `proposal` with the identity of its newest diff request."
  [proposal request-id]
  (assoc proposal :diff-request-id request-id))

(defn fork-diff-model
  "Presentation of a typed diff, independent of its settlement envelope.
   An adopted world may contain files or datoms; :world is not a data type.
   Missing counts are unknown, never evidence of an empty contribution."
  [{:keys [system-type counts files entries] :as fork}]
  (let [files? (or (#{:repo :review-world :file-output} system-type)
                   (and (= :world system-type)
                        (or (contains? fork :files) (contains? counts :files-changed))))
        kind (cond files? :files (= :book system-type) :entries :else :entities)]
    {:kind kind
     :label (case system-type
              :kb "Wiki" :fs "Drive" :book "Book" :repo "Code"
              :review-world "Workflow's work"
              :file-output "Retained files"
              :world (if files? "Run files" "Run data")
              (str system-type))
     :changed (case kind
                :files (or (:files-changed counts) (when (contains? fork :files) (count files)))
                :entries (when (contains? fork :entries) (count entries))
                (:entities-touched counts))}))

(defn settlement-supported?
  "Readiness, independent of permissions and live execution capabilities."
  [fork]
  (and (not= :file-output (:system-type fork))
       (not (false? (:settlement-supported? fork)))))

(defn dedicated-file-review?
  "Presentation only: one retained-file component uses the dedicated review
   path, never generic branch settlement. Does not imply compatibility or permission."
  [forks]
  (and (= 1 (count forks))
       (= :file-output (:system-type (first forks)))
       (nil? (:status (first forks)))))

(defn begin-audit-request [data pid request-id]
  (assoc-in data [:retained-audits pid] {:state :loading :request-id request-id}))

(defn apply-audit-result [data pid request-id result error]
  (if (not= request-id (get-in data [:retained-audits pid :request-id]))
    data
    (assoc-in data [:retained-audits pid]
              (cond
                error {:state :error :error error}
                (nil? result) {:state :unavailable}
                (and (= pid (:id result)) (#{:accepted :dismissed} (:status result))
                     (seq (:forks result))) {:state :ready :proposal result}
                :else {:state :error :error "No valid retained audit was returned"}))))

(defn begin-file-request [proposal scope path request-id]
  (assoc-in proposal [:file-bodies [scope path]] {:status :loading :request-id request-id}))

(defn begin-source-list [proposal scope token]
  (assoc-in proposal [:retained-sources scope] {:status :loading :list-token token}))

(defn hide-sources [proposal scope]
  (update proposal :retained-sources dissoc scope))

(defn apply-source-list [proposal scope token result error]
  (if (not= token (get-in proposal [:retained-sources scope :list-token]))
    proposal
    (assoc-in proposal [:retained-sources scope]
              (if (and (nil? error) (= :ok (:status result))
                       (captured/source-list-valid? (:id proposal) scope (:data result)))
                {:status :ready :list-token token :listing (:data result)}
                {:status :unavailable :list-token token}))))

(defn begin-source-read [proposal scope path token]
  (let [{:keys [status listing]} (get-in proposal [:retained-sources scope])
        entry (first (filter #(= path (:path %)) (:files listing)))]
    (if (and (= :ready status) entry)
      (assoc-in proposal [:retained-sources scope :read]
                {:status :loading :read-token token :entry entry :bytes []})
      proposal)))

(defn source-read-current? [proposal scope list-token read-token]
  (let [state (get-in proposal [:retained-sources scope])]
    (and (= :ready (:status state)) (= list-token (:list-token state))
         (= read-token (get-in state [:read :read-token]))
         (= :loading (get-in state [:read :status])))))

(defn apply-source-chunk [proposal scope list-token read-token result decoded error]
  (if-not (source-read-current? proposal scope list-token read-token)
    proposal
    (let [{:keys [listing read]} (get-in proposal [:retained-sources scope])
          bytes (:bytes read) chunk (:data result)
          valid? (and (nil? error) (= :ok (:status result)) (vector? decoded)
                      (every? #(and (integer? %) (<= 0 % 255)) decoded)
                      (captured/source-chunk-valid? listing (:entry read) (count bytes) chunk (count decoded)))]
      (assoc-in proposal [:retained-sources scope :read]
                (if valid?
                  (assoc read :bytes (into bytes decoded) :next-offset (:next-offset chunk)
                         :status (if (:next-offset chunk) :loading :received))
                  {:status :unavailable :read-token read-token :entry (:entry read)})))))

(defn finish-source-read [proposal scope list-token read-token text]
  (let [state (get-in proposal [:retained-sources scope]) read (:read state)]
    (if-not (and (= :ready (:status state)) (= list-token (:list-token state))
                 (= read-token (:read-token read)) (= :received (:status read)))
      proposal
      (assoc-in proposal [:retained-sources scope :read]
                (cond-> {:status (if (string? text) :ready :unavailable)
                         :read-token read-token :entry (:entry read)}
                  (string? text) (assoc :text text))))))

(defn begin-file-action
  "A review/action request belongs to this reactive card and exact component.
   Starting any request clears the old actionable revision."
  [proposal scope action request-id]
  (assoc proposal :file-adoption {:status :working :scope scope :action action :request-id request-id}))

(defn apply-file-action [proposal request-id result error]
  (let [{:keys [scope] :as current} (:file-adoption proposal)
        valid-review? (and (= :reviewed (:status result)) (= (:id proposal) (:proposal result))
                           (= scope (:component result)) (string? (:revision result))
                           (re-matches #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
                                       (:revision result)))]
    (if (not= request-id (:request-id current))
      proposal
      (assoc proposal :file-adoption
             (cond
               error {:status (if (#{:adopt :reconcile} (:action current)) :pending :unavailable)
                      :scope scope :error error}
               valid-review? {:status :reviewed :scope scope :revision (:revision result)}
               (#{:accepted :aborted :pending :review-changed :unavailable} (:status result))
               {:status (:status result) :scope scope}
               :else {:status :unavailable :scope scope})))))

(defn apply-file-result [proposal scope path request-id result error]
  (if (not= request-id (get-in proposal [:file-bodies [scope path] :request-id]))
    proposal
    (let [valid? (and (map? result) (= path (:path result)) (string? (:after result))
                      (or (nil? (:before result)) (string? (:before result))))]
      (assoc-in proposal [:file-bodies [scope path]]
                (if (or error (not valid?))
                  {:status :error :error (or error "No valid retained comparison was returned")}
                  {:status :ready :body result})))))

(defn apply-diff-result
  "Apply a diff result only when it belongs to the newest request.

   A failed newest request removes every derived field from an older success,
   so stale review material can never remain actionable under an error."
  [proposal request-id result error]
  (if (not= request-id (:diff-request-id proposal))
    proposal
    (if error
      (-> proposal
          (dissoc :diffs :tier :ai-summary :conflicts :checks)
          (assoc :diff-error error))
      (-> proposal
          (assoc :diffs (vec (:forks result))
                 :tier (:tier result)
                 :ai-summary (:summary result)
                 :conflicts (vec (:conflicts result)))
          (dissoc :diff-error)))))

(defn action-model
  "Pure compact-card projection of canonical Proposal review state.

   It deliberately consumes the same proposal map as the full inspector. No
   decision or mergeability semantics live in chat; this only derives labels
   and disabled states from the server-authoritative projection."
  [{:keys [status proposal]}]
  (let [forks (:forks proposal)
        blocked? (boolean
                  (some #(and (nil? (:status %))
                              (false? (:may-merge? %)))
                        forks))
        capability-missing? (boolean
                             (some #(and (nil? (:status %))
                                         (false? (:capability-live? %)))
                                   forks))
        review-only? (boolean (some #(and (nil? (:status %)) (not (settlement-supported? %))) forks))
        busy? (boolean (:busy? proposal))
        diff-ready? (some? (:diffs proposal))
        diff-error (:diff-error proposal)
        open? (= :open status)]
    {:status status
     :status-label (case status
                     :open "Open"
                     :accepted "Accepted"
                     :dismissed "Dismissed"
                     :checking "Checking…"
                     :error "Load failed"
                     "Unavailable")
     :busy? busy?
     :action-error (or (:error (when (= :error status) proposal))
                       (when diff-error
                         (str "Couldn't load changes — " diff-error))
                       (:action-error proposal))
     :retry? (boolean (or (= :error status) diff-error (:retry? proposal)))
     :accept-force? (boolean (:accept-warned? proposal))
     :accept-label (cond
                     busy? "Working…"
                     (dedicated-file-review? forks) "Use file review"
                     review-only? "Review only"
                     diff-error "Review unavailable"
                     (not diff-ready?) "Checking…"
                     capability-missing? "Unavailable"
                     blocked? "Cannot land all"
                     (:accept-warned? proposal) "Accept anyway"
                     :else "Accept")
     :accept-disabled? (or (not open?) busy? diff-error (not diff-ready?)
                           blocked? capability-missing? review-only?)
     :request-disabled? (or (not open?) busy?)
     :dismiss-disabled? (or (not open?) busy? capability-missing?)}))
