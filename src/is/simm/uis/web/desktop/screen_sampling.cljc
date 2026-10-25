(ns is.simm.uis.web.desktop.screen-sampling
  "Screenshot inference cadence. Continuous video recording is independent.")

(def minimum-interval-ms 20000)
(def signature-width 64)
(def signature-height 36)

(defn changed?
  "Meaningful difference from the last submitted frame. A pixel-change fraction
   catches text edits that a whole-screen average can miss. Unchanged screens
   do not trigger periodic model calls. Signatures are grayscale sequences."
  [before after]
  (cond
    (nil? before) true
    (not= (count before) (count after)) true
    (empty? after) false
    :else
    (let [[total changed]
          (reduce (fn [[total changed] [a b]]
                    (let [delta (#?(:clj Math/abs :cljs js/Math.abs) (double (- a b)))]
                      [(+ total delta) (+ changed (if (>= delta 24) 1 0))]))
                  [0.0 0] (map vector before after))
          n (count after)]
      (or (>= (/ total n) 6.0) (>= (/ changed n) 0.02)))))

(defn due?
  "First frame, or a changed frame after the minimum interval. Quiet periods
   cost no inference; the next change is compared with the submitted frame."
  [now last-post before after]
  (and (or (nil? last-post) (>= (- now last-post) minimum-interval-ms))
       (changed? before after)))

(defn initial [capture-id]
  {:capture-id capture-id :generation 0})

(defn refresh [state]
  (-> state (update :generation inc) (dissoc :signature)))

(defn request
  "Reserve one upload attempt, retaining the last accepted scene as baseline."
  [state now signature]
  (if (and (nil? (:pending state))
           (due? now (:last-attempt state) (:signature state) signature))
    (assoc state :last-attempt now
           :pending {:capture-id (:capture-id state) :generation (:generation state)
                     :at now :signature signature})
    state))

(defn acknowledge
  "Only acceptance advances the baseline. A new grant or capture invalidates
   an older acknowledgement; throttling retains the need for an observation."
  [state attempt accepted?]
  (if (= (:pending state) attempt)
    (cond-> (dissoc state :pending)
      (and accepted? (= (:generation state) (:generation attempt)))
      (assoc :signature (:signature attempt)))
    state))
