(ns is.simm.ops.github-push
  "Fixed HOST outbound publication. Prepare never sends; only a positive fresh
   Sending acknowledgement permits one send. Recovery observes, never resends."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [geschichte.git.budget :as budget]
            [geschichte.git.http :as http]
            [geschichte.git.push :as git]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-push :as push]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.github-ancestry :as ancestry]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-credentials :as credentials]
            [is.simm.ops.github-import :as import]
            [is.simm.ops.github-merge :as merge]
            [is.simm.ops.github-native :as native]
            [is.simm.ops.github-push-pack :as pack]
            [is.simm.ops.github-push-roots :as roots]
            [is.simm.ops.github-roots :as bases]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.benchmark-case-manifest :as manifests]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- refuse! [reason] (facts/refuse! reason))

(defn- send-fn [context] (or (:git-send context) http/send-http))

(defn- operation []
  (let [worker (Thread/currentThread) owner (:owner lifetime/*ticket*)]
    (budget/create
     {:limits (merge-with min (:limits (git/recommended-policy))
                          (:limits (merge/operation-policy)) import/limits)
      :timeout-ms 120000
      :cancelled? #(or (.isInterrupted worker) (not (lifetime/ready? owner)))})))

(defn- row! [context id]
  (let [row (journal/sync-in @(:conn context) id)]
    (when-not (and (= :push (journal/kind row))
                   (= (get-in context [:resolved :room-id]) (:github.sync/room row))
                   (or (:cleanup? context) (= (get-in context [:resolved :party]) (:github.sync/by row))))
      (refuse! :different-intent))
    row))

(defn- slot-id [slot body]
  (if (= :retention slot) (roots/body-id body) (facts/body-id (keyword "push" (name slot)) body)))

(defn- body! [context row slot]
  (let [prefix (if (= :preparation slot) "github.sync" "github.push")
        ref (get row (keyword prefix (name slot)))
        id (get row (keyword prefix (str (name slot) "-id")))
        body (:value (artifact/get-bounded-value (artifact/datahike-store (:conn context)) ref facts/max-artifact-bytes))]
    (when-not (and body (= id (slot-id slot body)) (= row (row! context (:github.sync/id row))))
      (refuse! :intent-changed))
    body))

(defn- cursor! [db expected]
  (integrations/validate-sync-base! db expected (:github.integration/head expected) (:github.integration/oid expected)))

(defn- transition!
  [context row state body {:keys [pack fact? extra]}]
  (let [slot ({:retaining :retention :ready :ready :sending :intent
               :pushed :outcome :rejected :outcome :observed :outcome :closed :closure} state)
        id (slot-id slot body) report (volatile! nil)
        check! #(if fact? (host/retained-fact! context) (host/current! context))]
    (check!)
    (artifact/publish-value!
     (artifact/datahike-store (:conn context)) body
     (fn [ref]
       (check!)
       (vreset! report
                (d/transact (:conn context)
                            [[:db.fn/call
                              (fn [db]
                                (if fact? (host/retained-fact! context) (host/writer-current! context db))
                                (into (push/transition-tx db row state ref id pack)
                                      (when extra (extra db ref id))))]]))))
    ;; Seeing the desired row is not a positive acknowledgement. In particular,
    ;; this is the only branch that may grant the caller permission to send.
    (when-not (and @report (= row (journal/sync-in (:db-before @report) (:github.sync/id row)))
                   (= state (:github.sync/state (journal/sync-in (:db-after @report) (:github.sync/id row)))))
      (refuse! :transition-unconfirmed))
    {:row (row! context (:github.sync/id row)) :report @report}))

(defn- preparation! [context row]
  (let [body (body! context row :preparation)]
    (when-not (and (= 1 (:version body))
                   (= (:binding body) {:id (:github.sync/id row) :by (:github.sync/by row)
                                       :room (:github.sync/room row) :client-id (:github.sync/client-id row)
                                       :claim (:github.sync/claim row)})
                   (= (:origin body) (:origin context))
                   (= (:revision body) (:revision context))
                   (= (:target body) (get-in context [:bound :target]))
                   (= (:target-stamp body) (get-in context [:bound :target-stamp])))
      (refuse! :source-settings-changed))
    (cursor! @(:conn context) (:cursor body))
    body))

(defn- prepare! [context request]
  (host/current! context)
  (let [control (:conn context) source (get-in context [:bound :conn])
        by (get-in context [:resolved :party]) room (get-in context [:resolved :room-id])
        id (journal/request-id by room (:client-id request))]
    (if-let [existing (journal/sync-in @control id)]
      (do (row! context id) existing)
      (let [operation (operation)
            _ (when-not (= (:expected-target-stamp request) (get-in context [:bound :target-stamp]))
                (refuse! :target-changed))
            {:keys [cursor connection]} (host/connection! context)
            before (facts/capture (:db (host/current! context)) (:origin context))
            head (get-in before [:visible :head])]
        (if (= head (:github.integration/head cursor))
          {:unchanged? true}
          (let [base (bases/retain-base! control @source connection by #(host/current! context) #(host/writer-current! context %))
                changes (facts/changes {:visible {:manifest (manifests/inventory (:db base))}} before)
                binding {:id id :by by :room room :client-id (:client-id request) :claim (random-uuid)}
                prepared {:version 1 :binding binding :cursor cursor :connection connection
                          :target (get-in context [:bound :target]) :target-stamp (:expected-target-stamp request)
                          :revision (:revision context) :origin (:origin context) :before before
                          :credential (:git-credential context)}
                ack (volatile! nil)]
            (artifact/publish-value!
             (artifact/datahike-store control) prepared
             (fn [ref]
               (host/current! context)
               (vreset! ack (d/transact control [[:db.fn/call
                                                  (fn [db] (host/writer-current! context db) (cursor! db cursor)
                                                    (credentials/writer-current! context db)
                                                    (push/prepare-tx db binding ref (slot-id :preparation prepared)))]]))))
            (when-not (and @ack (nil? (journal/sync-in (:db-before @ack) id))) (refuse! :creation-unconfirmed))
            (let [row (row! context id)
                  inspected (native/inspect! @source operation)
                  _ (when-not (= (:checkpoint before) (:checkpoint inspected)) (refuse! :target-changed))
                  url (get-in before [:visible :configuration "remote.origin.url"])
                  projected (git/prepare-http! (send-fn context) source url head
                                               {:ref (:ref connection) :upstream-commit (:github.integration/head cursor)
                                                :expected-remote-oid (:github.integration/oid cursor) :operation operation})
                  _ (when-not (= (:checkpoint before) (:source-checkpoint projected))
                      (refuse! :target-changed))
                  _ (host/current! context)
                  ;; Only generated Git identity metadata changes locally.
                  mapping (d/transact source [[:db.fn/call (fn [db] (git/mapping-tx db (dissoc projected :pack)))]])
                  source-db (:db-after mapping)
                  retention (roots/describe source-db row head (:new projected))
                  retained (transition! context row :retaining retention {})
                  _ (roots/create-original! context (:row retained) retention (:report retained))
                  ready (assoc (dissoc projected :pack :commits)
                               :version 1 :preparation-id (:github.sync/preparation-id row)
                               :retention-id (roots/body-id retention) :source-checkpoint (d/commit-id source-db)
                               :pack-size (long (alength ^bytes (:pack projected))) :changes changes)
                  result (volatile! nil)]
              (pack/publish!
               control (:pack projected)
               (fn [ref]
                 (vreset! result (:row (transition! context (:row retained) :ready (assoc ready :pack-ref ref)
                                                    {:pack {:github.push/pack ref :github.push/pack-size (:pack-size ready)}})))))
              @result)))))))

(defn- ready! [context row]
  (let [prepared (preparation! context row) ready (body! context row :ready)
        retention (body! context row :retention)]
    (when-not (and (= (:github.sync/preparation-id row) (:preparation-id ready))
                   (= (:github.push/retention-id row) (:retention-id ready))
                   (= (:pack-size ready) (:github.push/pack-size row))
                   (= (:pack-ref ready) (:github.push/pack row))
                   (= (:head ready) (:head retention)) (= (:new ready) (:git-oid retention))
                   (= (:source-checkpoint ready) (get-in retention [:roots :source :basis :commit]))
                   (= (:ref ready) (get-in prepared [:connection :ref]))
                   (= (:old ready) (get-in prepared [:cursor :github.integration/oid])))
      (refuse! :proof-changed))
    (roots/verify! context row retention)
    [prepared ready]))

(defn- apply! [context row request]
  (when-not (and (= :ready (:github.sync/state row)) (= (:ready-id request) (:github.push/ready-id row)))
    (refuse! :not-ready))
  (let [[prepared ready] (ready! context row) operation (operation)
        bytes (pack/read! (:conn context) (:github.push/pack row) (:pack-size ready) operation)
        source (:db (host/current! context))]
    (when-not (= (:credential prepared) (:git-credential context)) (refuse! :credential-changed))
    (when-not (= (:source-checkpoint ready) (d/commit-id source)) (refuse! :target-changed))
    (let [sending (:row (transition! context row :sending
                                     {:version 1 :ready-id (:github.push/ready-id row)
                                      :pack (:github.push/pack row) :old (:old ready) :new (:new ready)}
                                     {:extra (fn [db _ _] (cursor! db (:cursor prepared))
                                               (credentials/writer-current! context db) [])}))
          ;; No catch/retry. An exception or malformed response leaves Sending.
          outcome (git/send-http! (send-fn context)
                                  (get-in prepared [:before :visible :configuration "remote.origin.url"])
                                  (assoc ready :pack bytes) operation)
          _ (when-not (and (#{:pushed :rejected} (:status outcome))
                           (= (select-keys ready [:ref :old :new]) (select-keys outcome [:ref :old :new])))
              (refuse! :outcome-unconfirmed))
          body (assoc outcome :version 1 :kind (:status outcome) :ready-id (:github.push/ready-id row))]
      (:row (transition!
             context sending (:status outcome) body
             {:fact? true
              :extra (fn [db ref id]
                       (when (= :pushed (:status outcome))
                         (let [cursor (:cursor prepared)]
                           (integrations/advance-sync-tx db cursor (:github.sync/id row)
                                                         (:github.integration/head cursor) (:github.integration/oid cursor)
                                                         (:head ready) (:new ready) ref id))))})))))

(defn- close! [context row]
  (when-not (#{:creating :retaining :ready :pushed :rejected :observed :closed} (:github.sync/state row))
    (refuse! :outcome-unconfirmed))
  (if (= :closed (:github.sync/state row)) row
      (:row (transition! context row :closed {:version 1 :kind :closed :from (:github.sync/state row)
                                              :by (get-in context [:resolved :party])
                                              :retained-roots? (boolean (:github.push/retention row))} {}))))

(defn- recover! [context row]
  (case (:github.sync/state row)
    (:pushed :rejected :observed :closed) (close! context row)
    :sending
    (let [[prepared ready] (ready! context row) operation (operation)
          source (:db (host/current! context))
          current-head (get-in (facts/capture source (:origin context)) [:visible :head])
          _ (when-not (ancestry/proven-ancestor? source (:head ready) current-head operation)
              (refuse! :local-history))
          refs (http/ls-remote! (send-fn context)
                                (get-in prepared [:before :visible :configuration "remote.origin.url"])
                                {:prefixes [(:ref ready)] :import-budget operation})
          matches (filterv #(= (:ref ready) (:ref %)) refs)
          observed (:oid (first matches))]
      (when-not (and (= 1 (count matches)) (facts/git-oid? observed)
                     (#{(:old ready) (:new ready)} observed))
        (refuse! :remote-changed))
      (host/current! context)
      (let [body {:version 1 :kind (if (= observed (:new ready)) :upstream-observed :upstream-unchanged)
                  :ready-id (:github.push/ready-id row) :ref (:ref ready)
                  :head (:head ready) :observed-git-oid observed
                  :historical-send :unknown}
            result (:row (transition!
                          context row :observed body
                          {:extra (fn [db ref id]
                                    (if (= observed (:new ready))
                                      (integrations/observe-upstream-tx db (:cursor prepared) (:github.sync/id row)
                                                                        (:head ready) observed ref id)
                                      (do (cursor! db (:cursor prepared)) [])))}))]
        (close! context result)))
    (refuse! :recovery-held)))

(defn- dto [context row]
  (let [ready (when (:github.push/ready row) (body! context row :ready))
        outcome (when (:github.push/outcome row) (body! context row :outcome))
        state (:github.sync/state row)]
    (cond-> {:status (case state :sending :unconfirmed (:creating :retaining) :preparing state) :operation-kind :push
             :sync-id (str (:github.sync/id row)) :client-id (str (:github.sync/client-id row))
             :can-discard? (boolean (#{:creating :retaining :ready :pushed :rejected :observed} state))}
      ready (assoc :ready-id (str (:github.push/ready-id row)) :head (str (:head ready))
                   :before-git-oid (:old ready) :git-oid (:new ready) :pack-size (:pack-size ready)
                   :changes (:changes ready) :total (count (:changes ready)))
      outcome (assoc :outcome (:kind outcome)))))

(defn in!
  "Internal parsed requests inside the admitted original HOST. Browser actions
   go through github-service's human-owned credential admission; no arbitrary
   transport/context is accepted from the wire, and no agent API is installed."
  [context action request]
  (host/current! context)
  (let [id (or (:sync-id request)
               (journal/request-id (get-in context [:resolved :party]) (get-in context [:resolved :room-id]) (:client-id request)))
        row (if (= :prepare action) (prepare! context request) (journal/sync-in @(:conn context) id))]
    (cond
      (:unchanged? row) {:status :unchanged :operation-kind :push}
      (nil? row) (if (= :observe action) {:status :not-observed :operation-kind :push} (refuse! :different-intent))
      :else (let [row (row! context id)]
              (dto context (case action
                             (:prepare :observe) row
                             :apply (apply! context row request)
                             :recover (recover! context row)
                             :discard (close! context row)
                             :retire (do (when-not (:cleanup? context) (refuse! :cleanup-denied)) (close! context row))
                             (refuse! :invalid-request)))))))
