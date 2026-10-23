(ns is.simm.agents.office
  "Host-only, invocation-scoped Office adapter. Source selection shares the
   memory capability boundary; staged XML belongs to the selected world."
  (:require [clojure.string :as str]
            [dvergr.effects :as effects]
            [dvergr.sandbox :as sandbox]
            [geschichte.query :as query]
            [is.simm.agents.source-memory :as source]
            [is.simm.documents.files :as files]
            [is.simm.media.office :as office]
            [yggdrasil.protocols :as systems])
  (:import [java.util Base64]))

(def ^:private bytes-prefix "simmis-office-base64-v1:")
(def ^:private missing-bytes "simmis-office-missing-v1")

(defn- encode-bytes [bytes]
  (if bytes (str bytes-prefix (.encodeToString (Base64/getEncoder) bytes)) missing-bytes))

(defn- decode-bytes [result]
  (when-not (= missing-bytes result)
    (when-not (and (string? result) (str/starts-with? result bytes-prefix)
                   (<= (count result) (+ (count bytes-prefix) (* 4 (quot (+ office/max-open-bytes 2) 3)))))
      (throw (ex-info "Invalid or oversized Office byte receipt" {:error :invalid-receipt})))
    (let [bytes (try (.decode (Base64/getDecoder) ^String (subs result (count bytes-prefix)))
                     (catch IllegalArgumentException _
                       (throw (ex-info "Malformed Office byte receipt" {:error :invalid-receipt}))))]
      (when (> (alength bytes) office/max-open-bytes)
        (throw (ex-info "Office byte receipt exceeds the input limit" {:error :invalid-receipt})))
      bytes)))

(defn- refuse! [kind message]
  (throw (ex-info message {:error kind})))

(defn- legacy-path? [path]
  (and (string? path) (str/starts-with? path "/drive/")))

(defn- source-path! [path]
  (files/path! (if (and (string? path) (str/starts-with? path "/")) (subs path 1) path)))

(defn bindings
  "Repository reads use one immutable selected DB. Legacy /drive and binary
   publication explicitly refuse; neither can fall back to a shared root writer."
  [owner capability-id receipts policy]
  (let [boundary (sandbox/agent-effect-boundary owner capability-id nil receipts)
        with-workspace!
        (source/selection-resolver
         owner capability-id
         (assoc policy :with-selection!
                (fn [binding next!]
                  ;; Admission is effectful, local deterministic staging is not:
                  ;; a replayed access answer must never fabricate a live handle.
                  (effects/perform! boundary {:effect :room/read
                                              :resource {:op :office/access :room (str (:room-runtime-id binding))}}
                                    (constantly true))
                  (next!))))]
    (office/sci-namespace
     owner capability-id
     (fn [consume!]
       (with-workspace!
         (fn [{:keys [selected-context target canonical-system conn]}]
           (let [db @conn
                 source-path! (fn [path]
                                (when (legacy-path? path)
                                  (refuse! :unsupported-operation "Office reads the room source repository; legacy /drive access is unavailable"))
                                (source-path! path))
                 resource (fn [path]
                            {:path path :canonical-system (systems/system-id canonical-system)
                             :store-id (get-in db [:config :store :id]) :branch (get-in db [:config :branch])})
                 fs {:stat
                     (fn [path]
                       (let [path (source-path! path)]
                         (effects/perform!
                          boundary {:effect :fs/stat :resource (resource path)}
                          #(when-let [entry (files/entry db path)]
                             {:size (:size entry) :type :file}))))
                     :read-bytes
                     (fn [path]
                       (let [path (source-path! path)]
                         (decode-bytes
                          (effects/perform!
                           boundary {:effect :fs/read :resource (assoc (resource path) :representation :office-base64-v1)}
                           #(encode-bytes
                             (when-let [entry (files/entry db path)]
                               (files/source-bytes db path entry)))))))}]
             (consume! {:ctx selected-context :fs fs
                        :scope [capability-id (:room-id target) (:agent-id target)
                                (:scope target) (systems/system-id canonical-system)
                                (:geschichte.repo/id (query/repository db))]}))))))))
