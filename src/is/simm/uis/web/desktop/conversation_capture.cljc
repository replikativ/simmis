(ns is.simm.uis.web.desktop.conversation-capture
  "Closed wire receipts and pure context-owned evidence capture transitions.
   An uncertain publication is not a retryable preview, even after hiding it.")

(defn uuid-text? [s]
  (and (string? s)
       (boolean (re-matches #"(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" s))))

(defn path-for [id] (str "inbox/conversations/" id ".md"))

(defn- target? [r]
  (and (uuid? (:capture-id r)) (uuid? (:target-stamp r))
       (= (path-for (:capture-id r)) (:path r))))

(defn preview? [r]
  (and (map? r) (= :preview (:status r))
       (= #{:status :capture-id :target-stamp :path :text :message-ids} (set (keys r)))
       (target? r) (string? (:text r)) (<= 1 (count (:text r)) (* 512 1024))
       (vector? (:message-ids r)) (<= 1 (count (:message-ids r)) 50)
       (every? uuid? (:message-ids r))
       (= (count (:message-ids r)) (count (set (:message-ids r))))))

(defn saved? [r]
  (let [entry (:entry r)]
    (and (map? r) (= :saved (:status r))
         (= #{:status :capture-id :target-stamp :path :entry :commit} (set (keys r)))
         (target? r) (uuid-text? (:commit r))
         (map? entry) (= #{:content :size :mode} (set (keys entry)))
         (uuid? (:content entry)) (integer? (:size entry)) (<= 1 (:size entry) (* 512 1024))
         (= 33188 (:mode entry)))))

(defn reconcile [s connection]
  (if (or (nil? (:connection s)) (= connection (:connection s))) s
      (if (#{:publishing :inspection-required :saved} (:phase s))
        (cond-> (assoc s :token nil) (= :publishing (:phase s)) (assoc :phase :inspection-required))
        (-> s (assoc :phase :unavailable :token nil) (dissoc :preview)))))

(defn begin-preview [s connection token ids]
  (let [s (reconcile s connection)]
    (if (or (nil? connection) (#{:publishing :inspection-required :saved} (:phase s)))
      (assoc s :visible? true)
      {:phase :preview-loading :visible? true :connection connection :token token :message-ids ids})))

(defn current-request? [s connection token phase]
  (and (= phase (:phase s)) (= token (:token s)) (some? token)
       (some? connection) (= connection (:connection s)) (:visible? s)))

(defn receive-preview [s connection token reply]
  (if-not (current-request? s connection token :preview-loading) s
          (if (and (preview? reply) (= (:message-ids s) (:message-ids reply)))
            (assoc s :phase :preview :preview reply :token nil)
            (assoc s :phase :unavailable :token nil))))

(defn begin-publish [s connection token]
  (let [s (reconcile s connection)]
    (if (and (= :preview (:phase s)) (:visible? s) (preview? (:preview s)))
      (assoc s :phase :publishing :token token)
      s)))

(defn receive-publish [s connection token reply]
  (if-not (current-request? s connection token :publishing) s
          (let [p (:preview s)]
            (if (and (saved? reply) (= (select-keys p [:capture-id :path :target-stamp])
                                      (select-keys reply [:capture-id :path :target-stamp])))
              (assoc s :phase :saved :token nil :saved reply)
              (assoc s :phase :inspection-required :token nil)))))

(defn hide [s]
  (cond-> (assoc s :visible? false :token nil)
    (= :publishing (:phase s)) (assoc :phase :inspection-required)
    (= :preview-loading (:phase s)) (assoc :phase :idle)))
