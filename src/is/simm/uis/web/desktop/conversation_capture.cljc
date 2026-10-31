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

(defn room-artifact? [a room]
  (and (map? a) (= #{:message-id :source-revision :content-id :path :text :source-bytes} (set (keys a)))
       (uuid? (:content-id a)) (integer? (:source-bytes a)) (<= 0 (:source-bytes a) (* 256 1024))
       (uuid? (:message-id a)) (uuid? (:source-revision a))
       (= (:path a) (str "inbox/conversations/room-activity/" room "/" (:message-id a)
                         "/" (:source-revision a) ".md"))
       (string? (:text a)) (<= (count (:text a)) (* 512 1024))))

(defn room-preview? [r]
  (and (map? r) (= :room-preview (:status r))
       (every? #{:status :capture-id :target-stamp :source-room :source-store :artifacts
                 :state :complete? :limitations :scan-count :retained-count :held} (keys r))
       (uuid? (:capture-id r)) (uuid? (:target-stamp r)) (uuid? (:source-room r))
       (uuid? (:source-store r)) (#{:ready :bounded :held} (:state r))
       (boolean? (:complete? r)) (vector? (:artifacts r)) (<= (count (:artifacts r)) 20)
       (every? #(room-artifact? % (:source-room r)) (:artifacts r))))

(defn receive-room-preview [s connection token reply]
  (if-not (current-request? s connection token :preview-loading) s
          (if (room-preview? reply)
            (assoc s :phase :preview :preview reply :token nil)
            (assoc s :phase :unavailable :token nil))))

(defn begin-room-publish [s connection token]
  (let [s (reconcile s connection) p (:preview s)]
    (if (and (= :preview (:phase s)) (:visible? s) (room-preview? p)
             (not= :held (:state p)) (seq (:artifacts p)))
      (assoc s :phase :publishing :token token) s)))

(defn room-saved? [r p]
  (and (map? r) (= :room-saved (:status r))
       (= #{:status :capture-id :target-stamp :writes :state :complete? :limitations} (set (keys r)))
       (= (select-keys r [:capture-id :target-stamp]) (select-keys p [:capture-id :target-stamp]))
       (vector? (:writes r)) (= (count (:writes r)) (count (:artifacts p)))
       (every? true?
               (map (fn [w a]
                      (and (= #{:message-id :source-revision :path :saved? :commit} (set (keys w)))
                           (= (select-keys w [:message-id :source-revision :path])
                              (select-keys a [:message-id :source-revision :path]))
                           (true? (:saved? w)) (uuid-text? (:commit w))))
                    (:writes r) (:artifacts p)))))

(defn receive-room-publish [s connection token reply]
  (if-not (current-request? s connection token :publishing) s
          (if (room-saved? reply (:preview s))
            (assoc s :phase :saved :token nil :saved reply)
            (assoc s :phase :inspection-required :token nil))))

(defn begin-room-preview [s connection token ids]
  (begin-preview (if (and (= :saved (:phase s)) (= connection (:connection s)))
                   {:phase :idle} s)
                 connection token ids))
