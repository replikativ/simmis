(ns is.simm.uis.web.desktop.workspace-editor
  "Pure one-shot creation form. A missing ACK never becomes a resend intent."
  (:require [clojure.string :as str]
            [is.simm.documents.github-source :as github]))

(defn source [state]
  (when (= :github (:source-kind state))
    (try (github/source! {:kind :github :repository (:repository state)})
         (catch #?(:clj Exception :cljs :default) _ nil))))

(defn valid? [state]
  (and (string? (:name state)) (<= 1 (count (:name state)) 512)
       (not (str/blank? (:name state)))
       (or (nil? (:source-kind state))
           (and (= :github (:source-kind state)) (some? (source state))))
       (or (nil? (:members state)) (set? (:members state)) (vector? (:members state)))
       (<= (count (:members state)) 255)
       (every? #(and (string? %) (= % (some-> % parse-uuid str))) (:members state))))

(defn submit [state id socket]
  (if (or (:submission state) (not (uuid? id)) (nil? socket) (not (valid? state))) state
      (-> state
          (assoc :submission (cond-> {:id (str id) :name (str/trim (:name state))
                                      :members (vec (sort (set (:members state))))}
                               (source state) (assoc :source (source state)))
                 :submitted-on socket :phase :submitting)
          (dissoc :outcome :error))))

(defn created? [outcome]
  (and (= :created (:status outcome)) (= :project (get-in outcome [:room :room/type]))
       (let [id (get-in outcome [:room :room/id]) name (get-in outcome [:room :room/name])]
         (and (string? id) (= id (some-> id parse-uuid str))
              (string? name) (<= 1 (count name) 512)))))

(defn observed [state submission outcome]
  (if (not= submission (:submission state)) state
      (cond
        (created? outcome) (-> state (assoc :phase :created :outcome outcome) (dissoc :error))
        ;; A late pre-publication observation cannot undo an actual receipt.
        (created? (:outcome state)) state
        (contains? #{:not-observed :preparing :prepared-not-published :unavailable :conflict} (:status outcome))
        (assoc state :phase :unconfirmed :outcome (select-keys outcome [:status :correlation-id]))
        :else (assoc state :phase :unconfirmed :error "Creation could not be confirmed. Check this creation before starting another."))))
