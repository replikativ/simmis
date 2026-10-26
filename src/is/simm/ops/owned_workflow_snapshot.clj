(ns is.simm.ops.owned-workflow-snapshot
  "Bounded pure verification of an owned workflow's retained source bytes.
   No current DB, Room, provider, request, permission or execution authority."
  (:require [datahike.blob :as blob]
            [dvergr.catalog.room :as catalog]
            [dvergr.rooms.files :as native-files]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m])
  (:import [java.nio CharBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]))

(def entry-schema
  [:map {:closed true} [:path [:string {:min 1 :max 512}]] [:content :uuid]
   [:size [:int {:min 0 :max workflows/max-file-bytes}]] [:mode [:int {:min 0 :max Integer/MAX_VALUE}]]])

(def snapshot-schema
  [:map {:closed true}
   [:files [:map-of {:max workflows/max-files} [:string {:min 1 :max 512}]
            [:string {:max workflows/max-file-bytes}]]]
   [:manifest [:vector {:max workflows/max-files} entry-schema]]])
(def origin-schema
  [:map {:closed true} [:revision :uuid] [:target-stamp :uuid]
   [:target publication/target-schema]
   [:manifest [:vector {:max workflows/max-files} entry-schema]]])

(def deployment-schema
  ;; Shared retained structure. Invocation/policy admission remains the caller's
  ;; existing fixed domain gate, including workflow vs benchmark execution DTOs.
  [:map {:closed true}
   [:version [:= 2]] [:kind [:enum :workflow :benchmark]] [:room :uuid] [:by :uuid]
   [:cadence :map] [:execution :map] [:host-policy :map]
   [:model-policy [:map-of {:max 4} [:string {:min 1 :max 200}] :uuid]]
   [:creation-channel [:enum :host :mcp]]
   [:snapshot snapshot-schema] [:source-origin origin-schema]])

(defn- refuse! []
  (throw (ex-info "Retained owned workflow source is unavailable" {:type ::unavailable})))

(defn verify!
  "Verify the original source snapshot, not today's named workflow. Hashes
   are integrity facts; caller separately authorizes and verifies deployment."
  [room execution snapshot source-origin]
  (when-not (and (uuid? room) (m/validate snapshot-schema snapshot)
                 (m/validate origin-schema source-origin)
                 (m/validate workflows/name-schema (:name execution))
                 (uuid? (:expected-bundle execution)) (uuid? (:expected-owned-source execution)))
    (refuse!))
  (let [{:keys [files manifest]} snapshot
        _ (when-not (and (= (:manifest source-origin) manifest)
                         (= (mapv :path manifest) (vec (sort (keys files))))
                         (publication/host-target? (:target source-origin)))
            (refuse!))
        sizes
        (mapv (fn [{:keys [path size mode content]}]
                (let [encoder (doto (.newEncoder StandardCharsets/UTF_8)
                                (.onMalformedInput CodingErrorAction/REPORT)
                                (.onUnmappableCharacter CodingErrorAction/REPORT))
                      ;; EDN publication must not replace a malformed path's
                      ;; surrogate and silently change retained file identity.
                      _ (.encode encoder (CharBuffer/wrap ^String path))
                      buffer (.encode encoder (CharBuffer/wrap ^String (get files path)))
                      bytes (byte-array (.remaining buffer))
                      full-path (str "/workflows/" (:name execution) "/" path)]
                  (.get buffer bytes)
                  (when-not (and (= full-path (native-files/workspace-path full-path))
                                 (<= (count full-path) 512)
                                 (= 0x8000 (bit-and mode 0xF000))
                                 (= size (alength ^bytes bytes)) (= content (blob/blob-id bytes)))
                    (refuse!))
                  size)) manifest)
        source-id (sources/source-id {:room-id room :target-stamp (:target-stamp source-origin)}
                                    (:name execution) manifest)]
    (when-not (and (<= (reduce + 0 sizes) workflows/max-bundle-bytes)
                   (uuid? source-id)
                   (= source-id (:revision source-origin) (:expected-owned-source execution))
                   (= (:expected-bundle execution) (:id (catalog/bundle (:name execution) files))))
      (refuse!)))
  snapshot)
