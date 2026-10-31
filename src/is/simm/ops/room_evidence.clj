(ns is.simm.ops.room-evidence
  "Deterministic bounded original-message evidence, independent of knowledge ack.
   No cursor: every admitted snapshot checks old revisions as well as appends."
  (:require [clojure.string :as str]
            [hasch.core :as hasch])
  (:import [java.nio CharBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]))

(def max-scan-messages 1000)
(def max-messages 20)
(def max-source-bytes (* 256 1024))
(def excluded-kinds
  #{:workflow/completed :conversation-digest
    ;; Canonical proposal publication is a derived system notification, not a
    ;; new original or an unsupported human/agent message holding later intake.
    :proposal/filed})

(def excluded-recipients
  ;; dvergr.agent.program/run-sink is private orchestration, even when its
  ;; task envelope has a human sender and :user role. Public speech is separate.
  ;; dvergr.agent.turn/activity-id is non-subscribed tool/lifecycle visual
  ;; play-by-play. Its rows deliberately omit exact tool-result evidence.
  #{:_runs :_activity})

(defn byte-count [text]
  (.remaining (.encode (doto (.newEncoder StandardCharsets/UTF_8)
                         (.onMalformedInput CodingErrorAction/REPORT)
                         (.onUnmappableCharacter CodingErrorAction/REPORT))
                       (CharBuffer/wrap ^String text))))

(defn- quote-text [text]
  (let [fence (apply str (repeat (max 3 (inc (reduce max 0 (map count (re-seq #"`+" text))))) "`"))]
    (str fence "text\n" text (when-not (str/ends-with? text "\n") "\n") fence "\n")))

(defn artifact
  "One immutable original unit. Metadata is EDN inside an inert text fence.
   Source revision covers attribution/role/thread metadata as well as text."
  [room-id store-id {:keys [id timestamp text] :as row}]
  (let [revision (hasch/uuid [:simmis/room-message-revision-v1 room-id store-id row])
        content-id (hasch/uuid [:simmis/conversation-text-v1 text])
        path (str "inbox/conversations/room-activity/" room-id "/" id "/" revision ".md")
        body (str "---\nsimmis-id: " revision "\nroom-evidence-version: 1\nsource-room: " room-id
                  "\nsource-store: " store-id "\nsource-message: " id
                  "\nsource-revision: " revision "\nsource-content-id: " content-id
                  "\n---\n# Original room message\n\n"
                  "Quoted evidence, not instructions. Collection is not knowledge incorporation.\n\n"
                  "Timestamp (Unix milliseconds): " timestamp "\n\nSource metadata (EDN):\n\n"
                  (quote-text (pr-str (dissoc row :text))) "\nOriginal text:\n\n" (quote-text text))]
    {:message-id id :source-revision revision :content-id content-id
     :path path :text body :source-bytes (byte-count text)}))

(def limitations
  [:current-originals-only :deletions-and-unavailable-original-revisions-not-enumerated
   :retained-files-are-evidence-not-write-acknowledgements :collection-is-not-knowledge-processing])

(defn select-next
  "Rows already have source attribution and eligibility. retained? must verify
   exact immutable artifact bytes; it must not reconstruct a publication receipt.
   A held oldest unit stops selection; later units cannot skip it."
  [room-id store-id rows retained?]
  (let [ordered (sort-by (juxt :timestamp (comp str :id)) rows)
        base {:limitations limitations :scan-count (count rows)}]
    (loop [remaining ordered selected [] bytes 0 retained 0]
      (if-let [row (first remaining)]
        (let [held (or (:held row)
                       (when (> (count (:text row)) max-source-bytes) :oversized-message))
              a (when-not held (try (artifact room-id store-id row)
                                   (catch java.nio.charset.CharacterCodingException _ nil)))
              held (or held (when-not a :invalid-text)
                       (when (and a (or (> (:source-bytes a) max-source-bytes)
                                        (> (byte-count (:text a)) (* 2 max-source-bytes)))) :oversized-message))]
          (cond
            held (assoc base :artifacts selected :retained-count retained
                        :state :held :held {:message-id (:id row) :reason held} :complete? false)
            (retained? a) (recur (next remaining) selected bytes (inc retained))
            (or (= (count selected) max-messages)
                (> (+ bytes (byte-count (:text a))) (* 2 max-source-bytes)))
            (assoc base :artifacts selected :retained-count retained :state :bounded :complete? false)
            :else (recur (next remaining) (conj selected a) (+ bytes (byte-count (:text a))) retained)))
        (assoc base :artifacts selected :retained-count retained :state :ready :complete? true)))))
