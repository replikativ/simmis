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
                       (:action-error proposal))
     :accept-force? (boolean (:accept-warned? proposal))
     :accept-label (cond
                     busy? "Working…"
                     (not diff-ready?) "Checking…"
                     capability-missing? "Unavailable"
                     blocked? "Cannot land all"
                     (:accept-warned? proposal) "Accept anyway"
                     :else "Accept")
     :accept-disabled? (or (not open?) busy? (not diff-ready?)
                           blocked? capability-missing?)
     :request-disabled? (or (not open?) busy?)
     :dismiss-disabled? (or (not open?) busy? capability-missing?)}))
