(ns is.simm.ops.worker-broker
  "Serial inference endpoint; no room access, agent loop or tool execution."
  (:require [hasch.core :as hasch]
            [is.simm.ops.worker-frames :as frames]
            [is.simm.ops.worker-inference :as inference]
            [is.simm.ops.worker-wire :as wire]))

(defn- serve-once!
  "config and infer! are supplied by trusted LOCAL code, not guest messages.
   The outer runner must retain this process in the same accounted lifecycle
   as the guest and physically stop both before reporting work stopped."
  [ch config infer!]
  (let [exchange! (inference/broker config infer!)
        limit (+ 1024 (:max-frame-bytes inference/policy))]
    (frames/write! ch {"version" 1 "kind" "broker-ready" "config-id" (str (hasch/uuid config))
                       "inference-policy-id" (str (hasch/uuid inference/policy))} 4096)
    (loop []
      (when-let [frame (frames/read! ch limit)]
        (let [frame (wire/object! frame [:version :kind :request] [:version :kind])]
          (when-not (= 1 (:version frame)) (wire/refuse!))
          (case (:kind frame)
            "inference" (do
                          (when-not (contains? frame :request) (wire/refuse!))
                          (frames/write! ch {"version" 1 "kind" "inference-result"
                                             "reply" (wire/encode (exchange! (wire/request! (:request frame))))} limit)
                          (recur))
            "close" (do (when (contains? frame :request) (wire/refuse!))
                        (when (some? (frames/read! ch limit)) (wire/refuse!)))
            (wire/refuse!)))))))

(defn serve!
  "Serve a serial broker session with locally pinned config/inference. A
   malformed or ambiguous exchange permanently poisons this channel."
  [ch config infer!]
  (try (serve-once! ch config infer!)
       (catch Throwable _
         (reset! (:closed? ch) true)
         (wire/refuse!))))
