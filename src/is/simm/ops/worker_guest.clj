(ns is.simm.ops.worker-guest
  "Single-use credentialless guest process endpoint. Not a server entrypoint."
  (:require [dvergr.model.providers :as providers]
            [hasch.core :as hasch]
            [is.simm.ops.worker-executor :as executor]
            [is.simm.ops.worker-frames :as frames]
            [is.simm.ops.worker-inference :as inference]
            [is.simm.ops.worker-wire :as wire]))

(defn- serve-once!
  "Execute one verified frozen input in a FRESH guest-only process. Installing
   a provider here is process-owned I/O, never state in a forkable server room.
   ready/result are untrusted reports; neither proves containment or shutdown."
  [ch]
  (let [opening (wire/object! (frames/read! ch (:max-frame-bytes frames/policy))
                              [:version :kind :frozen] [:version :kind :frozen])]
    (when-not (and (= 1 (:version opening)) (= "execute" (:kind opening))) (wire/refuse!))
    (let [frozen (wire/frozen! (:frozen opening))
          exchange! (fn [request]
                      (frames/write! ch {"version" 1 "kind" "inference" "request" (wire/encode request)}
                                     (+ 1024 (:max-frame-bytes inference/policy)))
                      (let [reply (wire/object!
                                   (frames/read! ch (+ 1024 (:max-frame-bytes inference/policy)))
                                   [:version :kind :reply] [:version :kind :reply])]
                        (when-not (and (= 1 (:version reply)) (= "inference-result" (:kind reply))) (wire/refuse!))
                        (wire/reply! (:reply reply))))
          provider (inference/forward-provider (:config frozen) exchange!)]
      ;; Never clear an existing host registry or briefly expose login-backed
      ;; defaults. The caller must give this endpoint an unused guest process.
      (let [registered @providers/providers]
        (when-not (and (map? registered) (empty? registered)
                       (compare-and-set! providers/providers registered {:codex-subscription provider}))
          (wire/refuse!)))
      (frames/write! ch {"version" 1 "kind" "guest-ready" "input-id" (str (:input-id frozen))
                         "executor-policy-id" (str (hasch/uuid executor/policy))
                         "inference-policy-id" (str (hasch/uuid inference/policy))}
                     4096)
      (let [output (executor/execute! frozen)]
        (frames/write! ch {"version" 1 "kind" "result" "input-id" (str (:input-id frozen))
                           "output" (wire/encode output)} (:max-frame-bytes frames/policy))
        ;; After result the trusted parent closes input. EOF is coordination,
        ;; not physical stop proof. A second execute or stray reply is refused.
        (when (some? (frames/read! ch 4096)) (wire/refuse!))))))

(defn serve!
  "Serve a single guest session; every protocol/execution failure permanently
   poisons the channel. There is no resume, replacement provider or retry."
  [ch]
  (try (serve-once! ch)
       (catch Throwable _
         (reset! (:closed? ch) true)
         (wire/refuse!))))
