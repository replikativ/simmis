(ns is.simm.uis.web.desktop.room-digest
  "Pure account/room/view-owned optional digest state. Writes and launches are
   explicit; uncertainty stays sticky and socket changes never replay effects."
  (:require [clojure.string :as str]
            [is.simm.model.conversation-digest :as digest]
            [is.simm.model.room-summaries :as summaries]))

(defn reconcile [state connection]
  (if (or (nil? (:connection state)) (= connection (:connection state))) state
      (if (#{:installing :launching :publishing :inspection-required :submitted :published} (:phase state))
        (cond-> (assoc state :token nil)
          (#{:installing :launching :publishing} (:phase state)) (assoc :phase :inspection-required))
        (-> state (assoc :phase :unavailable :token nil) (dissoc :preview :result)))))

(defn begin [state connection token operation message]
  (let [state (reconcile state connection)]
    (if (or (and (= :publish operation) (:publication-pending? state))
            (and (#{:preview :install :launch} operation) (:effect-pending? state)))
      (assoc state :visible? true :message message)
      (cond-> (assoc state :phase (case operation :preview :preview-loading :install :installing :launch :launching :result :result-loading :publish :publishing)
             :operation operation :connection connection :token token :visible? true :message message)
        (#{:install :launch} operation) (assoc :effect-pending? true)
        (= :publish operation) (assoc :publication-pending? true)
        (#{:preview :launch} operation) (dissoc :request :result)))))

(defn current? [state connection token]
  (and (:visible? state) (some? connection) (some? token)
       (= connection (:connection state)) (= token (:token state))))

(defn- preview? [reply]
  (try
    (and (= :preview (:status reply))
         (uuid? (:room reply)) (uuid? (:target-stamp reply)) (uuid? (:source-revision reply))
         (digest/validate-selection! (:selection reply))
         (#{:installed :missing :unavailable} (:workflow reply))
         (map? (:inputs reply)) (<= (count (:inputs reply)) digest/max-sources)
         (every? #(and (string? %) (<= (count %) (* 256 1024))) (vals (:inputs reply))))
    (catch #?(:clj Exception :cljs :default) _ false)))

(defn- verified? [reply]
  (try
    (let [daily? (= :room-summary (:kind reply))
          selection (:selection reply)
          sources (if daily? (into (:coverage selection) (:context selection)) (:sources selection))]
      (and (= :verified (:status reply)) (uuid? (:request reply)) (uuid? (:digest-id reply))
           (if daily?
             (summaries/validate-report! selection (:digest reply))
             (and (digest/validate! selection (:digest reply))
                  (= (:digest-id reply) (digest/digest-id selection (:digest reply)))))
           (vector? (:originals reply))
           (= sources (mapv #(dissoc % :text) (:originals reply)))
           (every? #(and (string? (:text %)) (<= (count (:text %)) (* 512 1024))) (:originals reply))
           (<= (reduce + 0 (map #(count (:text %)) (:originals reply)))
               (if daily? summaries/max-input-bytes (* 256 1024)))))
    (catch #?(:clj Exception :cljs :default) _ false)))

(defn receive [state connection token reply]
  (if-not (current? state connection token) state
    (let [state (assoc state :token nil)]
      (case (:operation state)
        :preview (if (preview? reply) (assoc state :phase :preview :preview reply) (assoc state :phase :unavailable))
        :install (if (#{:installed :already-installed} (:status reply))
                   (assoc state :phase :installed :effect-pending? false) (assoc state :phase :inspection-required))
        :launch (if (and (= :submitted (:status reply)) (uuid? (:request reply)))
                  (assoc state :phase :submitted :request (:request reply))
                  (assoc state :phase :inspection-required :request (:request reply)))
        :result (if (and (= (:request state) (:request reply))
                         (or (nil? (:expected-digest-id state)) (= (:expected-digest-id state) (:digest-id reply)))
                         (verified? reply)) (assoc state :phase :verified :result reply :effect-pending? false)
                    (assoc state :phase :unavailable))
        :publish (if (and (= :published (:status reply)) (= (:request state) (:request reply))
                          (= (get-in state [:result :digest-id]) (:digest-id reply))
                          (uuid? (:message reply)))
                   (assoc state :phase :published :published reply :publication-pending? false)
                   (assoc state :phase :inspection-required))
        (assoc state :phase :unavailable)))))

(defn hide [state]
  (cond-> (assoc state :visible? false :token nil)
    (#{:installing :launching :publishing} (:phase state)) (assoc :phase :inspection-required)))

(defn message-context
  "Navigation hint from a room-owned retained path, never attribution proof."
  [room source]
  (let [parts (str/split (:path source "") #"/")
        id (nth parts 4 nil)]
    (when (and (= 6 (count parts)) (= ["inbox" "conversations" "room-activity" (str room)] (subvec parts 0 4))
               (string? id) (re-matches #"[0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}" id))
      {:kind :message :room (str room) :message id})))


(defn notification-reference
  "Persistent inert pointer, never a read capability or proof of provenance."
  [metadata]
  (let [object (:object metadata) digest-id (:id object) request (:notification/task metadata)]
    (when (and (or (= :conversation-digest (:kind metadata) (:kind object))
                   (= :room-summary (:kind metadata) (:kind object)))
               (= :task-complete (:notification/type metadata))
               (= (if (= :room-summary (:kind object)) :simmis/room-summary :simmis/conversation-digest)
                  (:source metadata))
               (uuid? request) (uuid? digest-id))
      {:request request :digest-id digest-id})))
