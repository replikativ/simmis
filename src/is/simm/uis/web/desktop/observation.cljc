(ns is.simm.uis.web.desktop.observation
  "Pure generation-aware observation state for context-owned UI projections.
   No timers, callback closures, native handles or process-global atoms. The
   caller keeps this data in its Spindel signal and performs requested I/O.
   A reply is observation only, never admission or retained authority.")

(defn offer
  "Observe the latest EPOCH and dependency GENERATION. The caller's epoch
   includes BOTH authenticated socket identity and owning execution-world id:
   copied worlds must not inherit an unacknowledgeable native in-flight slot.
   TOKEN is
   used only when starting a fetch. Returns [state request-or-nil]. An update
   during a fetch remains wanted until that original fetch acknowledges it;
   a new socket supersedes a hung old fetch without waiting for a timeout."
  [state epoch generation token]
  (let [state (if (= epoch (:epoch state)) state {:epoch epoch})
        state (assoc state :wanted generation)]
    (if (or (nil? epoch) (:active state)
            (and (contains? state :completed) (= generation (:completed state))))
      [state nil]
      (let [request {:epoch epoch :generation generation :token token}]
        [(assoc state :active request) request]))))

(defn owns?
  "Whether FRAME still owns the fetch slot. Used for acknowledgement even
   when newer dependencies mean its result must not be published."
  [state frame]
  (and (some? frame) (= frame (:active state)) (= (:epoch frame) (:epoch state))))

(defn current?
  "A matching fetch may publish only while its requested dependencies remain
   current. The caller must additionally check live session/socket/room scope."
  [state frame]
  (and (owns? state frame) (= (:generation frame) (:wanted state))))

(defn finish
  "Acknowledge only the original frame. A pending newer generation remains
   wanted and can start on the next offer. Only a published observation (or
   current failure) completes its generation. Unpublished replies must not
   suppress a later A→B→A fetch. Explicit retry or a new invalidation, not
   rendering, retries a current failure."
  ([state frame] (finish state frame (current? state frame)))
  ([state frame published?]
   (if (owns? state frame)
     (cond-> (dissoc state :active)
       published? (assoc :completed (:generation frame))
       (not published?) (dissoc :completed))
     state)))
