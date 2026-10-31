(ns is.simm.uis.conversation-capture-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.uis.web.desktop.conversation-capture :as m]))

(def id #uuid "01234567-0000-4000-8000-000000000000")
(def stamp #uuid "01234567-0000-4000-8000-000000000001")
(def message #uuid "01234567-0000-4000-8000-000000000002")
(def preview {:status :preview :capture-id id :target-stamp stamp :path (m/path-for id)
              :message-ids [message] :text "Inert <script>hostile()</script>"})
(def saved {:status :saved :capture-id id :target-stamp stamp :path (m/path-for id)
            :entry {:content id :size 100 :mode 33188} :commit (str message)})
(defn inspected [] (-> nil (m/begin-preview :socket :read [message])
                         (m/receive-preview :socket :read preview)))

(deftest wire-contracts-are-closed-and-target-bound
  (is (m/preview? preview)) (is (m/saved? saved))
  (doseq [reply [(assoc preview :authority true) (assoc preview :path "wiki/arbitrary.md")
                 (assoc preview :message-ids []) (assoc preview :message-ids [message message])
                 (assoc preview :capture-id (str id))]]
    (is (not (m/preview? reply))))
  (doseq [reply [(assoc saved :commit "not-a-commit") (assoc saved :path "other.md")
                 (assoc saved :entry {:content id :size 100 :mode 33261})
                 (assoc saved :authority true) (assoc saved :entry nil)]]
    (is (not (m/saved? reply)))))

(deftest only-current-inspected-selection-can-publish
  (let [loading (m/begin-preview nil :socket :read [message])]
    (is (= loading (m/receive-preview loading :old-socket :read preview)))
    (is (= loading (m/receive-preview loading :socket :old preview)))
    (is (= :unavailable (:phase (m/receive-preview loading :socket :read (assoc preview :message-ids [id])))))
    (is (= loading (m/begin-publish loading :socket :save))))
  (let [p (inspected) sending (m/begin-publish p :socket :save)]
    (is (= :preview (:phase p))) (is (= :publishing (:phase sending)))
    (is (= sending (m/begin-publish sending :socket :second)))
    (is (= :saved (:phase (m/receive-publish sending :socket :save saved))))
    (is (= sending (m/receive-publish sending :other :save saved)))
    (doseq [reply [nil {:status :inspection-required} (assoc saved :target-stamp id)
                   (assoc saved :capture-id message :path (m/path-for message))]]
      (is (= :inspection-required (:phase (m/receive-publish sending :socket :save reply)))))))

(deftest hiding-and-reconnecting-never-rearm-an-uncertain-publication
  (let [sending (m/begin-publish (inspected) :socket :save)]
    (doseq [held [(m/hide sending) (m/reconcile sending :next-socket)
                  (m/receive-publish sending :socket :save nil)]]
      (is (= :inspection-required (:phase held)))
      (let [again (m/begin-preview held :next-socket :new-read [message])]
        (is (= :inspection-required (:phase again)))
        (is (nil? (:token again)))
        (is (= again (m/begin-publish again :next-socket :new-save)))
        (is (= again (m/receive-publish again :socket :save saved))))))
  (let [p (m/hide (m/begin-preview nil :socket :read [message]))]
    (is (= p (m/receive-preview p :socket :read preview))))
  (let [s (m/receive-publish (m/begin-publish (inspected) :socket :save) :socket :save saved)]
    (is (= :saved (:phase (m/begin-preview s :next-socket :again [message]))))))

(def room-preview
  {:status :room-preview :capture-id id :target-stamp stamp :source-room stamp :source-store id
   :state :ready :complete? true :limitations [] :scan-count 1 :retained-count 0
   :artifacts [{:message-id message :source-revision id :content-id id :source-bytes 5
                :path (str "inbox/conversations/room-activity/" stamp "/" message "/" id ".md")
                :text "inert evidence"}]})

(deftest room-evidence-state-keeps-uncertain-acknowledgements-blocked
  (let [s (-> nil (m/begin-room-preview :socket :read [message])
              (m/receive-room-preview :socket :read room-preview))
        publishing (m/begin-room-publish s :socket :write)
        uncertain (m/receive-room-publish publishing :socket :write nil)]
    (is (m/room-preview? room-preview))
    (is (not (m/room-preview? (assoc room-preview :authority true))))
    (is (= :publishing (:phase publishing)))
    (is (= :inspection-required (:phase uncertain)))
    (is (= :inspection-required (:phase (m/begin-room-preview (m/hide uncertain) :socket :again [message]))))
    (is (= :preview (:phase (m/begin-room-publish (assoc-in s [:preview :state] :held) :socket :write))))
    (let [a (first (:artifacts room-preview))
          saved (assoc (select-keys room-preview [:capture-id :target-stamp :state :complete? :limitations])
                       :status :room-saved
                       :writes [(assoc (select-keys a [:message-id :source-revision :path])
                                       :saved? true :commit (str id))])
          complete (m/receive-room-publish publishing :socket :write saved)]
      (is (= :saved (:phase complete)))
      (is (= :preview-loading (:phase (m/begin-room-preview complete :socket :next [message])))))))
