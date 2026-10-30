(ns is.simm.ops.github-service
  "Transport entrypoint: new UI requests prepare incoming Proposals; existing
   fast-forward attempts retain their protocol and are never reinterpreted."
  (:require [is.simm.model.github-sync :as journal]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-incoming :as incoming]
            [is.simm.ops.github-sync :as legacy]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.file-output-adoption :as adoption]))

(defn prepare [party room operation wire-request]
  (let [action (if (= :prepare-incoming operation) :prepare operation)
        request (legacy/request! action wire-request)]
    (host/capture
     party room action
     (fn [context]
       (let [id (or (:sync-id request) (journal/request-id party room (:client-id request)))
             row (journal/sync-in @(:conn context) id)
             incoming? (or (= :prepare-incoming operation) (= :incoming (journal/kind row)))]
         (if incoming?
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
           (legacy/in! context action request)))))))
