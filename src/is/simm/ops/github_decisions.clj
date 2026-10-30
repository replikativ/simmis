(ns is.simm.ops.github-decisions
  "Exact Git Proposal review and human settlement inside the original HOST.
   Proposal status is the decision authority. A durable applying record fences
   native publication; unknown outcomes never permit replay or dismissal."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [geschichte.git.budget :as budget]
            [hasch.core :as hasch]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-proposals :as provenance]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.github-candidate-roots :as roots]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-merge :as merge]
            [is.simm.ops.github-native :as native]
            [is.simm.ops.github-review :as review]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- refuse! [reason]
  (throw (ex-info "Git Proposal decision is held" {:type ::held :reason reason})))

(defn- proposal-in [db id]
  (d/pull db '[* {:proposal/forks [*]}] [:proposal/id id]))

(defn- filing-value [proposal]
  (assoc (select-keys proposal [:proposal/id :proposal/title :proposal/summary :proposal/created-at
                                :proposal/filing-hash :proposal/author :proposal/room :proposal/github-key
                                :proposal/github-provenance :proposal/github-provenance-id :proposal/github-replaces])
         :proposal/forks
         (mapv #(select-keys % [:db/id :proposal.fork/scope :proposal.fork/authority-scope :proposal.fork/branch
                                :proposal.fork/system-type :proposal.fork/author
                                :proposal.fork/base-commit :proposal.fork/head-commit]) (:proposal/forks proposal))))

(defn- decision-value [proposal]
  (select-keys proposal [:proposal/github-decision :proposal/github-decision-id :proposal/github-decision-state]))

(defn- evidence! [context id]
  (let [control (:conn context) source (:db (host/current! context))
        proposal (proposal-in @control id)
        body (:value (artifact/get-bounded-value (artifact/datahike-store control)
                                                 (:proposal/github-provenance proposal) 32768))]
    (when-not (= (get-in context [:resolved :room-id]) (:proposal/room proposal)) (refuse! :different-room))
    (provenance/validate-proposal! body proposal)
    (let [row (journal/sync-in @control (get-in body [:producer :sync-id]))
          filing (:value (artifact/get-bounded-value (artifact/datahike-store control) (:github.sync/filing row) (* 2 1024 1024)))
          original (:proposal filing)]
      (when-not (and (= (filing-value original) (update (filing-value proposal) :proposal/forks
                                                       #(mapv (fn [fork] (dissoc fork :db/id)) %)))
                     (= (get-in body [:identity :connection :target]) (get-in context [:bound :target]))
                     (= (get-in body [:identity :connection :target-stamp]) (get-in context [:bound :target-stamp]))
                     (= (provenance/provenance-id body) (:provenance-id filing))
                     (= :closed (:github.sync/state row)) (:github.sync/outcome row))
        (refuse! :filing-changed))
      (roots/verify! control source row body #(host/current! context))
      {:proposal proposal :body body
       :cursor (:cursor (:value (artifact/get-bounded-value (artifact/datahike-store control)
                                                            (:github.sync/preparation row) (* 2 1024 1024))))
       :canonical source
       :local (d/commit-as-db source (get-in body [:roots :local :basis :commit]))
       :candidate (d/commit-as-db source (get-in body [:roots :candidate :basis :commit]))})))

(defn- review-in! [context id]
  (let [{:keys [proposal body cursor canonical local candidate] :as evidence} (evidence! context id)]
    (when-not (= :open (:proposal/status proposal)) (refuse! :proposal-closed))
    (when (= :applying (:proposal/github-decision-state proposal)) (refuse! :outcome-unconfirmed))
    (integrations/validate-sync-base! @(:conn context) cursor (:github.integration/head cursor) (:github.integration/oid cursor))
    (assoc evidence :review (review/inspect canonical local candidate body))))

(defn- matching-claim! [db expected applying]
  (let [current (proposal-in db (:proposal/id expected))]
    (when-not (and (= (filing-value expected) (filing-value current))
                   (= :open (:proposal/status current)) (= applying (decision-value current)))
      (refuse! :decision-changed))
    current))

(defn- accept-in! [context id review-id]
  (let [{:keys [proposal body cursor candidate review]} (review-in! context id)
        _ (when-not (and (= :ready (:status review)) (= review-id (:review-id review))) (refuse! :review-changed))
        control (:conn context) parent (get-in context [:bound :conn])
        actor (get-in context [:resolved :party])
        intent {:version 1 :claim (random-uuid) :proposal id :by actor :room (:proposal/room proposal)
                :provenance-id (provenance/provenance-id body) :review review-id
                :checkpoint (get-in review [:basis :canonical :commit]) :cursor cursor}
        digest (hasch/uuid [:simmis/github-decision-v1 intent])
        ack (volatile! nil) applying (volatile! nil)]
    (artifact/publish-value!
     (artifact/datahike-store control) intent
     (fn [ref]
       (host/current! context)
       (let [claim {:proposal/github-decision ref :proposal/github-decision-id digest :proposal/github-decision-state :applying}]
         (vreset! ack
                  (d/transact control
                              [[:db.fn/call (fn [db]
                                              (host/writer-current! context db)
                                              (let [current (proposal-in db id)]
                                                (when-not (and (= (filing-value proposal) (filing-value current))
                                                               (= :open (:proposal/status current))
                                                               (= (decision-value proposal) (decision-value current))
                                                               (not= :applying (:proposal/github-decision-state current)))
                                                  (refuse! :decision-changed))
                                                (integrations/validate-sync-base! db cursor (:github.integration/head cursor) (:github.integration/oid cursor))
                                                [(assoc claim :db/id (:db/id current))]))]]))
         (vreset! applying claim))))
    ;; A readback never supplies dispatch rights after a lost/ambiguous ACK.
    (when-not (and @ack @applying (= @applying (decision-value (proposal-in (:db-after @ack) id))))
      (refuse! :decision-unconfirmed))
    (let [result
          (try
            (let [worker (Thread/currentThread) owner (:owner lifetime/*ticket*)
                  operation (budget/create (assoc (merge/operation-policy) :timeout-ms 120000
                                                   :cancelled? #(or (.isInterrupted worker) (not (lifetime/ready? owner)))))
                  source (reify clojure.lang.IDeref (deref [_] candidate))]
              (native/advance! source parent (get-in body [:identity :connection :ref]) (:checkpoint intent)
                               operation #(host/current! context)))
            (catch Exception error
              (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
              (when (#{:geschichte.workspace/stale-advance :is.simm.ops.github-native/held
                        :geschichte.git.budget/exceeded :geschichte.git.budget/deadline-exceeded
                        :geschichte.git.budget/cancelled} (:type (ex-data error)))
                ;; These fixed native/preflight refusals happen before effect.
                ;; Record only that fact, with the original admitted claim.
                (d/transact control [[:db.fn/call
                                      (fn [db]
                                        (let [current (matching-claim! db proposal @applying)]
                                          [{:db/id (:db/id current) :proposal/github-decision-state :refused}]))]]))
              (throw error)))]
      (when-not (and (= (get-in body [:proposal :base]) (:old result))
                     (= (get-in body [:proposal :head]) (:new result))
                     (= (get-in body [:roots :candidate :basis :commit]) (:canonical-commit result)))
        (refuse! :outcome-unconfirmed))
      (let [outcome {:version 1 :kind :accepted :proposal id :claim (:claim intent) :by actor
                     :decision-id digest :native result :review review-id}
            outcome-id (hasch/uuid [:simmis/github-outcome-v1 outcome]) at (java.util.Date.)
            final (volatile! nil)]
        (artifact/publish-value!
         (artifact/datahike-store control) outcome
         (fn [ref]
           (vreset! final
                    (d/transact control
                                [[:db.fn/call
                                  (fn [db]
                                    ;; Finalize the original observed positive
                                    ;; effect, even if permissions changed after
                                    ;; publication. No new effect/authorization.
                                    (let [current (matching-claim! db proposal @applying)
                                          fork (first (:proposal/forks current))]
                                      (into [{:db/id (:db/id current) :proposal/status :accepted
                                              :proposal/resolved-by actor :proposal/resolved-at at
                                              :proposal/github-decision-state :accepted
                                              :proposal/github-outcome ref :proposal/github-outcome-id outcome-id}
                                             {:db/id (:db/id fork) :proposal.fork/status :accepted
                                              :proposal/resolved-by actor :proposal/resolved-at at}]
                                            (integrations/advance-sync-tx db cursor (get-in body [:producer :sync-id])
                                                                          (:github.integration/head cursor) (:github.integration/oid cursor)
                                                                          (get-in body [:identity :incoming :head])
                                                                          (get-in body [:identity :incoming :git-oid]) ref outcome-id))))]]))))
        (when-not (and @final (= :accepted (:proposal/status (proposal-in (:db-after @final) id))))
          (refuse! :outcome-unconfirmed))
        {:status :accepted :proposal-id (str id)}))))

(defn- dismiss-in! [context id]
  (let [{:keys [proposal]} (evidence! context id)
        actor (get-in context [:resolved :party]) at (java.util.Date.)]
    (d/transact (:conn context)
                [[:db.fn/call (fn [db]
                                (host/writer-current! context db)
                                (let [current (proposal-in db id)]
                                  (when-not (and (= (filing-value current) (filing-value proposal))
                                                 (= :open (:proposal/status current))
                                                 (not= :applying (:proposal/github-decision-state current)))
                                    (refuse! :decision-changed))
                                  [{:db/id (:db/id current) :proposal/status :dismissed
                                    :proposal/resolved-by actor :proposal/resolved-at at}
                                   {:db/id (:db/id (first (:proposal/forks current))) :proposal.fork/status :dismissed
                                    :proposal/resolved-by actor :proposal/resolved-at at}]))]])
    {:status :dismissed :proposal-id (str id)}))

(defn in! [context action request]
  (case action
    :review (let [result (:review (review-in! context (:proposal-id request)))]
              (assoc (select-keys result [:status :reason :review-id :changes :diagnostics]) :proposal-id (str (:proposal-id request))))
    :accept (accept-in! context (:proposal-id request) (:review-id request))
    :dismiss (dismiss-in! context (:proposal-id request))))

(defn prepare [party room action request]
  (let [fields (if (= :accept action) #{:proposal-id :review-id} #{:proposal-id})]
    (when-not (and (#{:review :accept :dismiss} action) (= fields (set (keys request)))
                   (every? #(and (string? %) (= % (some-> % parse-uuid str))) (vals request)))
      (refuse! :invalid-request))
    (host/capture party room action #(in! % action (update-vals request parse-uuid)))))
