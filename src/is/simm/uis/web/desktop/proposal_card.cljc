(ns is.simm.uis.web.desktop.proposal-card)

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
                     diff-error "Review unavailable"
                     (not diff-ready?) "Checking…"
                     capability-missing? "Unavailable"
                     blocked? "Cannot land all"
                     (:accept-warned? proposal) "Accept anyway"
                     :else "Accept")
     :accept-disabled? (or (not open?) busy? diff-error (not diff-ready?)
                           blocked? capability-missing?)
     :request-disabled? (or (not open?) busy?)
     :dismiss-disabled? (or (not open?) busy? capability-missing?)}))
