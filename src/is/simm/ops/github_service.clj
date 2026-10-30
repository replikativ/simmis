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
  (let [action (if (#{:prepare-incoming :prepare-push} operation) :prepare operation)
        request (legacy/request! action wire-request)]
    {:action action :request request}))

(defn- dispatch! [context operation {:keys [action request]}]
  (let [party (get-in context [:resolved :party]) room (get-in context [:resolved :room-id])
        id (or (:sync-id request) (journal/request-id party room (:client-id request)))
        row (journal/sync-in @(:conn context) id)
        incoming? (or (= :prepare-incoming operation) (= :incoming (journal/kind row)))]
    (cond
      (or (= :prepare-push operation) (= :push (journal/kind row)))
      (try (if (or (#{:prepare :apply} action)
                   (and (= :recover action) (= :sending (:github.sync/state row))
                        (:configured? (credentials/status! context))))
             (credentials/with-transport! context #(push/in! % action request))
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
      (try (let [prepared (incoming/in! context action request)]
                  ;; One human Sync request prepares and files a clean proposal.
                  ;; Acceptance remains separate. Filing has its OWN fresh,
                  ;; positively acknowledged admission before native publication.
             (if (and (= :prepare-incoming operation) (= :ready (:status prepared)))
               (incoming/in! context :apply {:client-id (:client-id request) :ready-id (parse-uuid (:ready-id prepared))})
               prepared))
           (catch Exception error
             (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)) (throw error))
                  ;; No preparation exists: report that explicitly rather than
                  ;; trapping the UI in unconfirmed -> not-observed forever.
             (let [observed (journal/sync-in @(:conn context) id)]
               (cond
                 (and (= :prepare action) (nil? observed))
                 {:status :not-started :operation-kind :incoming :client-id (str (:client-id request))
                  :reason (facts/error-reason error)}
                 (= :held (:github.sync/state observed))
                 (incoming/in! context :observe {:client-id (:client-id request)})
                 :else (throw error)))))
      :else (legacy/in! context action request))))

(defn prepare-credential
  "Authenticated owner-only settings entry. No token, envelope or actor is
   returned; the observed revision is required for replacement/revocation."
  [party room operation request]
  (let [fields (case operation :status #{:expected-target-stamp}
                     :save #{:connection-id :revision :token} :revoke #{:connection-id :revision} nil)
        revision (:revision request)]
    (when-not (and fields (map? request) (= fields (set (keys request)))
                   (or (nil? revision)
                       (and (string? revision) (= revision (some-> revision parse-uuid str))))
                   (let [id (if (= :status operation) (:expected-target-stamp request) (:connection-id request))]
                     (and (string? id) (= id (some-> id parse-uuid str))))
                   (or (not= :save operation)
                       (and (string? (:token request)) (<= 20 (count (:token request)) 512))))
      (facts/refuse! :invalid-request))
    (host/capture party room (if (= :status operation) :observe :review)
                  (fn [context]
                    (case operation
                      :status (do (when-not (= (parse-uuid (:expected-target-stamp request)) (get-in context [:bound :target-stamp]))
                                    (facts/refuse! :target-changed))
                                  (credentials/status! context))
                      :save (credentials/save! context (parse-uuid (:connection-id request)) (some-> revision parse-uuid) (:token request))
                      :revoke (credentials/revoke! context (parse-uuid (:connection-id request)) (some-> revision parse-uuid)))))))

(defn in!
  "Synchronous fixed entry inside an original retained App operation. Scheduled
   callers supply that original host; this never captures a replacement App.
   The same human authorization, Room lease and Git admission remain required."
  [original-host party room operation wire-request]
  (let [request (request! operation wire-request)]
    (host/in! original-host party room (:action request) #(dispatch! % operation request))))

(defn prepare
  "Capture the original App once before queuing an authenticated UI operation."
  [party room operation wire-request]
  (let [request (request! operation wire-request)]
    (host/capture party room (:action request) #(dispatch! % operation request))))
