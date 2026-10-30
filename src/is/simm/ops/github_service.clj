(ns is.simm.ops.github-service
  "Transport entrypoint: new UI requests prepare incoming Proposals; existing
   fast-forward attempts retain their protocol and are never reinterpreted."
  (:require [is.simm.model.github-sync :as journal]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-incoming :as incoming]
            [is.simm.ops.github-push :as push]
            [is.simm.ops.github-credentials :as credentials]
            [is.simm.ops.github-sync :as legacy]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.file-output-adoption :as adoption]))

(defn- request! [operation wire-request]
  (let [action (if (#{:prepare-incoming :prepare-policy :prepare-push} operation) :prepare operation)
        request (legacy/request! action wire-request)]
    {:action action :request request}))

(defn- dispatch! [context operation {:keys [action request]}]
  (let [party (get-in context [:resolved :party]) room (get-in context [:resolved :room-id])
        id (or (:sync-id request) (journal/request-id party room (:client-id request)))
        row (journal/sync-in @(:conn context) id)
        incoming-kind (if row
                        (when (#{:incoming :incoming-policy} (journal/kind row)) (journal/kind row))
                        (case operation :prepare-incoming :incoming :prepare-policy :incoming-policy nil))
        incoming? (some? incoming-kind)]
    (cond
      (or (= :prepare-push operation) (= :push (journal/kind row)))
      (try (if (or (#{:prepare :apply} action)
                   (and (= :recover action) (= :sending (:github.sync/state row))
                        (credentials/selected? context)))
             (credentials/with-transport! (assoc context :require-personal-credential? (and (= :prepare action) (nil? row))) #(push/in! % action request))
        ;; Sources currently must be public. Without a configured credential,
        ;; explicit recovery can still read public refs. A selected credential
        ;; failure never retries anonymously. Terminal cleanup needs no token.
             (push/in! context action request))
           (catch Exception error
             (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)) (throw error))
             (if (and (= :prepare action) (nil? (journal/sync-in @(:conn context) id)))
               {:status :not-started :operation-kind :push :client-id (str (:client-id request))
                :reason (facts/error-reason error)}
               (throw error))))
      incoming?
      (let [context (if (= :incoming-policy incoming-kind)
                      (incoming/admitted-context (assoc context :incoming-kind incoming-kind)) context)]
        (try
          (if-let [pending (when (and (= :incoming-policy incoming-kind) (= :prepare action) (nil? row))
                             (incoming/pending-proposal! context))]
            {:status :filed :operation-kind :incoming-policy :proposal-id (str pending)}
            (let [prepared (incoming/in! context action request)]
              ;; A single admitted operation prepares and either advances a
              ;; verified FF or files a normal proposal, according to its
              ;; immutable journal kind/disposition. No second admission.
              (if (and (#{:prepare-incoming :prepare-policy} operation) (= :ready (:status prepared)))
                (incoming/in! context :apply {:client-id (:client-id request) :ready-id (parse-uuid (:ready-id prepared))})
                prepared)))
          (catch Exception error
            (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)) (throw error))
            (let [observed (journal/sync-in @(:conn context) id)]
              (cond
                (and (= :prepare action) (nil? observed))
                {:status :not-started :operation-kind incoming-kind :client-id (str (:client-id request))
                 :reason (facts/error-reason error)}
                (= :held (:github.sync/state observed))
                (incoming/in! context :observe {:client-id (:client-id request)})
                :else (throw error))))))
      :else (legacy/in! context action request))))

(defn prepare-credential
  "Authenticated owner-only settings entry. No token, envelope or actor is
   returned; the observed revision is required for replacement/revocation."
  [party room operation request]
  (let [fields (case operation :status #{:expected-target-stamp}
                     :select #{:expected-target-stamp}
                     :bind #{:connection-id :revision :account-id :account-revision} :revoke #{:connection-id :revision} nil)
        revision (:revision request)]
    (when-not (and fields (map? request) (= fields (set (keys request)))
                   (or (nil? revision)
                       (and (string? revision) (= revision (some-> revision parse-uuid str))))
                   (let [id (if (#{:status :select} operation) (:expected-target-stamp request) (:connection-id request))]
                     (and (string? id) (= id (some-> id parse-uuid str))))
                   (or (not= :bind operation)
                       (every? #(and (string? %) (= % (some-> % parse-uuid str))) [(:account-id request) (:account-revision request)])))
      (facts/refuse! :invalid-request))
    (host/capture party room (if (#{:status :select} operation) :observe :review)
                  (fn [context]
                    (case operation
                      :status (do (when-not (= (parse-uuid (:expected-target-stamp request)) (get-in context [:bound :target-stamp]))
                                    (facts/refuse! :target-changed))
                                  (credentials/status! context))
                      :select (do (when-not (= (parse-uuid (:expected-target-stamp request)) (get-in context [:bound :target-stamp]))
                                    (facts/refuse! :target-changed))
                                  (credentials/selection! context))
                      :bind (credentials/bind! context (parse-uuid (:connection-id request)) (some-> revision parse-uuid)
                                               (parse-uuid (:account-id request)) (parse-uuid (:account-revision request)))
                      :revoke (do (credentials/revoke! context (parse-uuid (:connection-id request)) (some-> revision parse-uuid))
                                  (credentials/selection! context)))))))

(defn in!
  "Synchronous fixed entry inside an original retained App operation. Scheduled
   callers supply that original host; this never captures a replacement App.
   The same human authorization, Room lease and Git admission remain required."
  [original-host party room operation wire-request]
  (let [request (request! operation wire-request)]
    (host/in! original-host party room (:action request) #(dispatch! % operation request))))

(defn poll-in!
  "Fixed scheduler entry in ONE already admitted original Git context. Never
   recapture an App or nest its native lease/semaphore; never dispatch push."
  [context frozen-policy]
  (dispatch! (assoc context :poll-policy frozen-policy) :prepare-policy
             {:action :prepare :request {:client-id (random-uuid)
                                        :expected-target-stamp (get-in context [:bound :target-stamp])}}))

(defn prepare
  "Capture the original App once before queuing an authenticated UI operation."
  [party room operation wire-request]
  (let [request (request! operation wire-request)]
    (host/capture party room (:action request) #(dispatch! % operation request))))
