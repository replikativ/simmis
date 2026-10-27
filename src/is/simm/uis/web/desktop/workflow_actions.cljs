(ns is.simm.uis.web.desktop.workflow-actions
  "Browser workflow I/O; all state and callbacks retain their owning context."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            [is.simm.documents.workflow-links :as workflow-links]
            [is.simm.uis.web.desktop.observation-io :as observation-io]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.memory-remote :as memory-remote]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.runtimes.web :as web]))

(defn state-key
  ([room] (state-key room nil))
  ([room tab] (cond-> [(:id (web/current-session)) (str room)] tab (conj tab))))

(defn confirm! [message] (js/confirm message))

(defn- submitter [backend mode]
  (case [backend mode]
    ["cli" "benchmark"] remote/submit-cli-workflow-benchmark!
    ["local" "benchmark"] remote/submit-local-workflow-benchmark!
    ["local" "run"] remote/submit-local-workflow-run!
    ["hosted" "benchmark"] remote/submit-workflow-benchmark!
    ["hosted" "run"] remote/submit-workflow-run!
    (throw (js/Error. "Unknown execution backend or mode"))))

(defn controller
  "Capture the rendering context, rather than using a process-global loader."
  [room-data]
  (let [ctx (or ec/*execution-context* runtime)
        session (web/current-session)
        room (str (:room-id room-data))
        canonical (when (= :chat-workflow (first (:workflow-state-owner room-data)))
                    (second (:workflow-state-owner room-data)))
        k (state-key room (or (:workflow-state-owner room-data) (:tab-id room-data)))
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (web/current-session? session)
                      (apply swap! sig/workflow-editor-state update k f args))))
        state (fn [] (binding [ec/*execution-context* ctx] (get @sig/workflow-editor-state k)))
        observe! (observation-io/controller ctx session state update!)
        authority-generation (fn []
                               (binding [ec/*execution-context* ctx]
                                 (let [stream @sync/state]
                                   [(connection/snapshot) (:fork-id (ec/home-context ctx))
                                    (:status stream) (:connection stream) (:control-dirty stream)
                                    (get-in stream [:dirty room]) @sig/user-rooms])))
        generation (fn []
                     (binding [ec/*execution-context* ctx]
                       [(authority-generation) (:retry (state)) (:stream-registered (state))
                        (:selected (state))
                        (select-keys (:requests (state)) [[:save (:selected (state))]
                                                          [:source (:selected (state))]])]))
        verification-generation (fn [] [(authority-generation) (:retry (state)) (:stream-registered (state))])
        readiness-generation (fn []
                               [(authority-generation) (:retry (state)) (:stream-registered (state))
                                (:readiness-revision (state))])
        stream-generation (fn [] [(authority-generation) (:retry (state)) (:stream-registered (state))
                                  (editor/stream-request (state))])
        change! (fn [name f & args] (apply update! update-in [:drafts name] f args))
        request!
        (fn [slot selected invoke consume & [stale-consume]]
          (when (and (web/current-session? session) (connection/snapshot))
            (let [slot (if selected [slot selected] slot)
                  token (str (random-uuid)) socket (connection/snapshot)
                  captured-authority (authority-generation)
                  captured-roster (binding [ec/*execution-context* ctx] @sig/user-rooms)
                  current? #(and (web/current-session? session)
                                 (connection/current? socket)
                                 (editor/current-response? (state) slot token nil))
                  finish! (fn [f & [acknowledge]]
                            (binding [ec/*execution-context* ctx]
                              (when (current?)
                                (update! (fn [s]
                                           (if (= captured-authority (authority-generation)) (f s)
                                           ;; Acknowledge our transport slot but
                                           ;; never restore stale source/access.
                                               (-> (if (and acknowledge (= captured-roster @sig/user-rooms)) (acknowledge s) s)
                                                   (assoc-in [:loading slot] false)
                                                   (dissoc :permissions)
                                                   (update :drafts
                                                           (fn [drafts] (into {} (map (fn [[n d]] [n (editor/unverified d)])) drafts))))))))))]
              (update! #(-> % (assoc-in [:requests slot] token)
                            (assoc-in [:loading slot] true) (update :errors dissoc slot)))
              (try
                (binding [ec/*execution-context* ctx ec/*spin-id* nil]
                  (let [s (invoke)]
                    (s (fn [reply]
                         (try
                           (finish! #(-> (consume % reply) (assoc-in [:loading slot] false))
                                    (when stale-consume #(stale-consume % reply)))
                           (catch :default error
                             (finish! #(-> % (assoc-in [:loading slot] false)
                                           (assoc-in [:errors slot] (rem/error-text error)))))))
                       (fn [error]
                         (finish! #(-> % (assoc-in [:loading slot] false)
                                       (assoc-in [:errors slot] (rem/error-text error))))))))
                (catch :default error
                  (finish! #(-> % (assoc-in [:loading slot] false)
                                (assoc-in [:errors slot] (rem/error-text error)))))))))]
    (letfn [(offer-generation []
              [(authority-generation) (:readiness-status (state)) (:readiness (state)) (:permissions (state))])
            (verified-offer! [name offered]
              (let [draft (get-in (state) [:drafts name])]
                (when-not (and (some? (ec/home-context ctx))
                               (identical? (ec/home-context ctx) (ec/home-context runtime))
                               (web/current-session? session) (connection/snapshot)
                               (= :ready (:readiness-status (state)))
                               (:merge? (:permissions (state)))
                               (not (:access-denied? (state)))
                               (= (:generation offered) (offer-generation))
                               (= offered (:setup-offer draft))
                               (= (:form offered) (:form draft))
                               (nil? (:submission draft)) (nil? (:schedule-submission draft))
                               (= (dissoc offered :generation)
                                  (editor/setup-offer draft (:readiness (state)) (:action offered))))
                  (throw (js/Error. "Setup changed. Review the current proposal before confirming.")))
                offered))
            (observe-all! []
              (let [socket (connection/snapshot)
                    owner [socket (:fork-id (ec/home-context ctx))]
                    roster (binding [ec/*execution-context* ctx] @sig/user-rooms)
                    allowed? (or (nil? roster)
                                 (some #(= room (str (:room/id %))) (:rooms roster)))
                    navigation [(:workflow-name room-data) (:workflow-action room-data)]
                    navigation? (and (:tab-id room-data) (workflow-links/name? (:workflow-name room-data))
                                     (contains? workflow-links/actions (:workflow-action room-data))
                                     (not= (:applied-navigation (state)) navigation))
                    _ (when navigation?
                        (update! assoc :selected (:workflow-name room-data)
                                       :applied-navigation navigation :pending-navigation navigation))
                    selected (:selected (state))
                    _ (when (not= owner (:observation-owner (state)))
                        ;; Retire transport bookkeeping, NOT original request
                        ;; payloads kept with each draft for identical retries.
                        (update! #(-> % (assoc :observation-owner owner :loading {} :requests {})
                                      (dissoc :activations :activation-status :readiness :readiness-status :workflow-stream :workflow-stream-status :observed-stream-generation))))
                    gen (generation) verification-gen (verification-generation)]
                (when (not= (not allowed?) (:access-denied? (state)))
                  (update! assoc :access-denied? (not allowed?)))
                (when (not= gen (:observed-generation (state)))
                  (update! (fn [s]
                             (cond-> (assoc s :observed-generation gen)
                               (not= gen (get-in s [:observations :activations :completed]))
                               (assoc :activation-status :checking)))))
                (when (not= verification-gen (:observed-verification-generation (state)))
                  ;; Selecting a tab does not change its saved source. Only
                  ;; authority/source invalidation hints retire verification;
                  ;; otherwise returning to a completed source can stay stale
                  ;; while its identical observation generation suppresses IO.
                  (update! (fn [s]
                             (-> s (assoc :observed-verification-generation verification-gen)
                                 (update :drafts (fn [drafts] (into {} (map (fn [[n d]] [n (editor/unverified d)])) drafts)))))))
                (let [read-gen (readiness-generation)]
                  (when (not= read-gen (:observed-readiness-generation (state)))
                    (update! (fn [s]
                               (cond-> (assoc s :observed-readiness-generation read-gen)
                                 (not= read-gen (get-in s [:observations :readiness :completed]))
                                 (assoc :readiness-status :checking))))))
                (when (or (nil? socket) (not allowed?))
                  ;; Retire read frames as well as their projections. If the
                  ;; exact same roster returns, its old completed generation
                  ;; must not suppress a fresh authorized observation. Keep
                  ;; all command/uncertain-setup journals in the drafts.
                  (update! #(dissoc % :permissions :list :activations :activation-status :readiness :readiness-status
                                   :observations :observation-accepted :workflow-stream :workflow-stream-status :observed-stream-generation)))
                (when (and socket allowed?)
                  (observe! :stream #(vector (:retry (state)))
                            #(memory-remote/prepare-memory-stream! web/server-id room)
                            (fn [s _] (-> s (update :errors dissoc :stream)
                                          (update :stream-registered (fnil inc 0))))
                            (fn [s error] (assoc-in s [:errors :stream] error))
                            (fn [_] (sync/ensure! socket)))
                  (observe! :catalog generation
                            #(remote/load-workflow-list! web/server-id room {:offset 0})
                            (fn [s reply] (-> s (assoc :list reply :permissions (:permissions reply))
                                              (update :errors dissoc :list)))
                            (fn [s error] (-> s (dissoc :permissions :list) (assoc-in [:errors :list] error))))
                  (observe! :readiness readiness-generation
                            #(remote/load-workflow-readiness! web/server-id room)
                            (fn [s reply] (-> s (assoc :readiness reply :readiness-status :ready)
                                                  (update :drafts (fn [drafts]
                                                                    (into {} (map (fn [[n d]] [n (editor/automatic-model d reply)])) drafts)))
                                              (update :errors dissoc :readiness)))
                            (fn [s _] (-> s (dissoc :readiness) (assoc :readiness-status :unavailable)
                                          (assoc-in [:errors :readiness] "Model and allowance observations are unavailable. Retry updates when the connection is ready."))))
                  (if-let [request (editor/stream-request (state))]
                    (let [gen (stream-generation)]
                      (when (not= gen (:observed-stream-generation (state)))
                        (update! assoc :observed-stream-generation gen :workflow-stream-status :checking))
                      (observe! :workflow-stream stream-generation
                                #(remote/load-workflow-stream-status! web/server-id room request)
                                (fn [s reply] (editor/observe-stream s request reply))
                                (fn [s _] (-> s (dissoc :workflow-stream)
                                              (assoc :workflow-stream-status :unavailable)))))
                    (when (or (:workflow-stream (state)) (:workflow-stream-status (state))
                              (:observed-stream-generation (state))
                              (get-in (state) [:observations :workflow-stream]))
                      (update! editor/forget-stream)))
                  (observe! :wiki-setup generation
                            #(remote/inspect-wiki-maintenance-setup! web/server-id room)
                            editor/observe-wiki-setup
                            (fn [s _] (editor/observe-wiki-setup s {:status :unavailable})))
                  (observe! :activations generation
                            #(remote/load-workflow-schedules! web/server-id room)
                            (fn [s rows] (assoc s :activations rows :activation-status :ready))
                            (fn [s _] (-> s (dissoc :activations) (assoc :activation-status :unavailable))))
                  (when (and (= selected (first (:pending-navigation (state))))
                           (get-in (state) [:drafts selected :source]))
                  (update! #(-> %
                                (update-in [:drafts selected] editor/choose-action (second (:pending-navigation %)))
                                (dissoc :pending-navigation))))
                (when (and selected
                             (not (get-in (state) [:loading [:save selected]]))
                             (not (get-in (state) [:loading [:source selected]])))
                    (observe! [:source selected] generation
                              #(remote/load-workflow-source! web/server-id room selected)
                              (fn [s reply] (-> s (assoc :permissions (:permissions reply))
                                                (update-in [:drafts selected] #(editor/automatic-model (editor/observed % reply) (:readiness s)))
                                                (update :errors dissoc [:source selected])))
                              (fn [s error] (-> s (dissoc :permissions)
                                                (assoc-in [:errors [:source selected]] error))))))))
            (load-list! [offset]
              (let [prior (:list (state))]
                (request! :list nil
                          #(remote/load-workflow-list! web/server-id room
                                                       (cond-> {:offset offset}
                                                         (pos? offset) (assoc :expected-revision (:revision prior))))
                          (fn [s reply]
                            (assoc s :permissions (:permissions reply)
                                   :list (cond-> reply (pos? offset) (update :names #(into (:names prior) %))))))))
            (load-workers! [after]
              (let [prior (:workers (state))]
                (request! :workers nil
                          #(remote/load-local-workflow-workers! web/server-id room (cond-> {} after (assoc :after after)))
                          (fn [s reply]
                            (assoc s :workers (cond-> reply after (update :workers #(into (:workers prior) %))))))))
            (select! [name reload? & [create?]]
              (when (or (nil? canonical) (= canonical name))
               (update! #(-> % (assoc :selected name) (dissoc :pending-navigation)))
              (when (and (not (get-in (state) [:loading [:save name]]))
                         (not (get-in (state) [:loading [:source name]]))
                         (or reload? (nil? (get-in (state) [:drafts name :source]))))
                (request! :source name #(remote/load-workflow-source! web/server-id room name)
                          (fn [s reply] (-> s (assoc :permissions (:permissions reply))
                                            (assoc-in [:drafts name]
                                                      (cond-> (editor/reloaded (get-in s [:drafts name]) reply)
                                                        (and create? (empty? (:files reply)))
                                                        (assoc :files editor/scaffold :path "workflow.edn")))))))))
            (save! [name]
              (when-not (or (get-in (state) [:loading [:save name]])
                            (get-in (state) [:loading [:source name]]))
                (let [draft (get-in (state) [:drafts name])
                      sent (:files draft)
                      edits {:name name :expected-revision (get-in draft [:source :revision])
                             :files (editor/edits draft)}]
                  (request! :save name #(remote/save-workflow-source! web/server-id room edits)
                            (fn [s reply]
                            ;; Keep edits typed while the save was in flight.
                              (-> s (assoc :permissions (:permissions reply))
                                  (update-in [:drafts name] editor/acknowledged-save (:expected-revision edits) sent reply)))
                            ;; A post-durable hint can precede its own ACK. Sent
                            ;; bytes + original target-bound base may advance
                            ;; this draft, but cannot restore read permission or
                            ;; mark the new default verified.
                            (fn [s reply]
                              (update-in s [:drafts name] editor/acknowledged-save (:expected-revision edits) sent reply))))))
            (submit! [name retry? & [offered]]
              (let [draft (cond-> (get-in (state) [:drafts name]) offered (assoc :form (:form offered)))
                    pending (:submission draft)]
                (try
                  (when (or (get-in (state) [:loading [:submit name]])
                            (and (not retry?) pending (nil? (:reply pending))))
                    (throw (js/Error. "Retry the existing unresolved request instead of creating another")))
                  (when offered (verified-offer! name offered))
                  (let [payload (if retry? (:payload pending) (editor/submission draft (str (random-uuid))))
                        mode (if retry? (:mode pending) (get-in draft [:form :mode]))
                        backend (if retry? (get pending :backend "hosted") (get-in draft [:form :backend] "hosted"))]
                    (when-not payload (throw (js/Error. "There is no request to retry")))
                    (when-not (or retry? offered
                                  (confirm!
                                   (str "Submit " (:name payload) " at bundle " (:expected-bundle payload)
                                        "?\n" (cond (= backend "cli")
                                                    (str "Claude Code text under Dvergr SCI; "
                                                         (get-in payload [:limits :max-cli-runs]) " CLI Run admissions; "
                                                         (get-in payload [:limits :max-model-steps]) " steps per cell. No resume/compaction. Not an HTTP or dollar cap.\n")
                                                    (= backend "local")
                                                    (str "Local worker " (:worker payload) "; "
                                                         (* (max 1 (count (:cases payload))) (get payload :repetitions 1))
                                                         " cells/admissions. Usage unreported.\n")
                                                    :else (str "Budget: " (get-in payload [:limits :budget-microdollars])
                                                               " microdollars; " (get-in payload [:limits :max-dispatches]) " dispatches.\n"))
                                        (if (= mode "benchmark") "Wall time per cell: " "Wall time: ")
                                        (get-in payload [:limits :timeout-ms]) " ms"
                                        (when (= backend "local") " (includes cold startup)") ".\n"
                                        (when-let [split (:split payload)]
                                          (str "Evaluation split: " split
                                               (when (= split "held-out") " — acceptance evaluation, not prompt tuning") ".\n"))
                                        (if (= mode "benchmark")
                                          "Operator benchmark: executes the saved checker, uses saved parameter defaults and discards candidate worlds."
                                          (if (= backend "local")
                                            "Only pinned workflow/source inputs reach the local guest. Completion is not a correctness check; nothing auto-merges."
                                            "The Run inherits room files. Frozen workflow/source inputs appear in its proposed diff. Completion is not a correctness check; nothing auto-merges.")))))
                      (throw (js/Error. "Submission cancelled")))
                    (change! name assoc :form (:form draft) :submission
                             (if retry? pending
                                 {:payload payload :mode mode :backend backend
                                  :knowledge? (contains? (get-in draft [:source :files]) "input-selection.edn")}))
                    (request! :submit name
                              #((submitter backend mode) web/server-id room payload)
                              (fn [s reply] (-> s (assoc-in [:drafts name :submission :reply] (dissoc reply :permissions))
                                                (update :readiness-revision (fnil inc 0))))
                              (fn [s reply] (-> s (assoc-in [:drafts name :submission :reply] (dissoc reply :permissions))
                                                (update :readiness-revision (fnil inc 0))))))
                  (catch :default error (update! assoc-in [:errors :submit] (rem/error-text error))))))
            (schedule! [name retry? & [enabled? offered]]
              (try
                (let [draft (cond-> (get-in (state) [:drafts name]) offered (assoc :form (:form offered)))
                      pending (:schedule-submission draft)]
                  (when offered (verified-offer! name offered))
                  (when (or (get-in (state) [:loading [:schedule name]])
                            (and (not retry?) pending (nil? (:reply pending))))
                    (throw (js/Error. "Retry the unresolved schedule request first")))
                  (let [payload (if retry? (:payload pending)
                                    (editor/scheduled-submission draft (str (random-uuid)) (if (nil? enabled?) true enabled?)))]
                    (when-not payload (throw (js/Error. "No schedule request to retry")))
                    (when (or retry? offered (false? (:enabled? payload))
                              (confirm! (str "Activate recurring " (:kind payload) " · " name "?\n"
                                             (editor/cadence-label (:cadence payload)) ".\n"
                                             "Pins this saved source and current execution settings. Each occurrence uses the shared room allowance; no automatic funding or merging. Overlapping occurrences are skipped.")))
                      (change! name assoc :form (:form draft) :schedule-submission (if retry? pending {:payload payload}))
                      (request! :schedule name #(remote/create-workflow-schedule! web/server-id room payload)
                                (fn [s reply]
                                  (-> s (assoc-in [:drafts name :schedule-submission :reply] (dissoc reply :permissions))
                                         (update :readiness-revision (fnil inc 0))))
                                (fn [s reply] (-> s (assoc-in [:drafts name :schedule-submission :reply] (dissoc reply :permissions))
                                                (update :readiness-revision (fnil inc 0))))))))
                (catch :default error (update! assoc-in [:errors :schedule] (rem/error-text error)))))
            (inspect-setup! []
              (request! :wiki-setup-inspect nil
                        #(remote/inspect-wiki-maintenance-setup! web/server-id room)
                        editor/observe-wiki-setup))
            (install-setup! []
              (when (and (= :missing (get-in (state) [:wiki-setup :status]))
                         (not (:access-denied? (state)))
                         (confirm! "Install the wiki maintenance workflow in this room? This changes only the empty starter workflow folder. It does not choose a model, fund work, enable a schedule or merge any output."))
                (update! editor/begin-wiki-setup)
                (request! :wiki-setup-install nil
                          #(remote/install-wiki-maintenance! web/server-id room)
                          editor/acknowledge-wiki-setup editor/acknowledge-wiki-setup)))
            (upgrade-setup! []
              (let [setup (:wiki-setup (state))
                    payload {:expected-revision (:revision setup) :from-version (:installed-version setup)}]
                (when (and (= :upgrade-available (:status setup)) (not (:access-denied? (state))))
                  (update! editor/begin-wiki-setup payload)
                  (request! :wiki-setup-upgrade nil
                            #(remote/upgrade-wiki-maintenance! web/server-id room payload)
                            editor/acknowledge-wiki-setup editor/acknowledge-wiki-setup))))
            (collect-output! [name]
              (when-let [request-id (editor/collectable-output-request (get-in (state) [:drafts name]))]
                (when-not (get-in (state) [:loading [:output name]])
                  (let [acknowledge #(editor/acknowledge-output %1 name request-id %2)]
                    ;; Filing publishes a control/room hint before its own ACK.
                    ;; Retain only that request's receipt across invalidation;
                    ;; request! still fences changed sessions/rosters and drops
                    ;; permissions and source verification on the stale path.
                    (request! :output name #(remote/collect-workflow-wiki-output! web/server-id room request-id)
                              acknowledge acknowledge)))))
            (fund! [name]
              (try
                (when (get-in (state) [:loading [:fund name]])
                  (throw (js/Error. "Allowance request already in progress")))
                (let [draft (get-in (state) [:drafts name])
                      backend (if (and (not (:funded? draft)) (:fund-request draft))
                                (get draft :fund-backend "hosted")
                                (get-in draft [:form :backend] "hosted"))
                      payload (or (when-not (:funded? draft) (:fund-request draft))
                                  (editor/allowance (:form draft) (str (random-uuid))))]
                  (when (confirm! (str "Provision execution allowance (not a payment): "
                                       (if (= backend "cli")
                                         (str (:max-cli-runs payload) " single-use CLI Run admissions (not HTTP dispatches or dollars)?")
                                         (if (= backend "local")
                                           (str (:max-worker-admissions payload) " local worker admissions (not dollars/tokens)?")
                                           (str (:budget-microdollars payload) " microdollars and "
                                                (:max-dispatches payload) " dispatches?")))))
                    (change! name assoc :fund-request payload :fund-backend backend :funded? false)
                    (request! :fund name #((case backend
                                             "cli" remote/provision-cli-workflow-allowance!
                                             "local" remote/provision-local-workflow-allowance!
                                             remote/provision-workflow-allowance!) web/server-id room payload)
                              (fn [s reply] (-> s (assoc-in [:drafts name :funded?] true)
                                                (assoc-in [:drafts name :fund-reply] (dissoc reply :permissions))
                                                (update :readiness-revision (fnil inc 0))))
                              (fn [s reply] (-> s (assoc-in [:drafts name :funded?] true)
                                                (assoc-in [:drafts name :fund-reply] (dissoc reply :permissions))
                                                (update :readiness-revision (fnil inc 0)))))))
                (catch :default error (update! assoc-in [:errors :fund] (rem/error-text error)))))]
      {:observe observe-all!
       :refresh #(do (update! update :retry (fnil inc 0))
                     (binding [ec/*execution-context* ctx]
                       (sync/retry! (connection/snapshot))))
       :more #(load-list! (get-in (state) [:list :next-offset]))
       :workers #(load-workers! nil) :more-workers #(load-workers! (get-in (state) [:workers :next-after]))
       :select #(select! % false) :reload #(select! % true)
       :new-name #(update! assoc :new-name %)
       :new-authoring-open #(update! assoc :new-authoring-open? %)
       :create (fn []
                 (let [name (:new-name (state))]
                   (if (and (string? name) (re-matches #"[a-z0-9][a-z0-9-]{0,63}" name))
                     (select! name (nil? (get-in (state) [:drafts name])) true)
                     (update! assoc-in [:errors :source] "Use a lowercase workflow name with letters, digits and dashes (max64)."))))
       :prepare-setup (fn [name action]
                        (try
                          (let [draft (get-in (state) [:drafts name])
                                offered (assoc (editor/setup-offer draft (:readiness (state)) action)
                                               :generation (offer-generation))]
                            (change! name assoc :form (:form offered) :execution-open? false
                                     :schedule-open? (= :schedule (:action offered))
                                     :setup-form-revision (get-in draft [:source :revision])
                                     :setup-default-model (when (#{:user-preference :deployment-default} (:model-source offered))
                                                            (get-in offered [:form :model]))
                                     :setup-offer offered :setup-dismissed? false)
                            (update! update :errors dissoc :setup))
                          (catch :default error (update! assoc-in [:errors :setup] (rem/error-text error)))))
       :offer-current? (fn [name offered] (try (verified-offer! name offered) true (catch :default _ false)))
       :accept-setup (fn [name offered]
                       (try
                         (verified-offer! name offered)
                         (case (:action offered)
                           :schedule (schedule! name false true offered)
                           :run (submit! name false offered)
                           (throw (js/Error. "Review a valid setup proposal before confirming.")))
                         (catch :default error (update! assoc-in [:errors :setup] (rem/error-text error)))))
       :open-advanced (fn [name]
                        (let [draft (get-in (state) [:drafts name])]
                          (when (and draft (nil? (:submission draft)) (nil? (:schedule-submission draft))
                                     (not= (:setup-form-revision draft) (get-in draft [:source :revision])))
                            (try
                              (let [configuration (editor/setup-configuration draft (:readiness (state)) nil)]
                                (change! name assoc :form (:form configuration)
                                         :schedule-open? (= :schedule (:action configuration))
                                         :setup-form-revision (get-in draft [:source :revision])
                                         :setup-default-model (when (#{:user-preference :deployment-default} (:model-source configuration))
                                                                (get-in configuration [:form :model]))))
                              (catch :default error (update! assoc-in [:errors :setup] (rem/error-text error)))))
                          (when draft (change! name assoc :execution-open? true)))
                        (binding [ec/*execution-context* ctx]
                          (when (web/current-session? session)
                            (update! assoc :selected name)
                            (sig/open-tab! :workflows
                                           (assoc room-data :workflow-state-owner (or (:workflow-state-owner room-data) (:tab-id room-data)))
                                           {:title "Workflow · Advanced" :new-tab? true :col-id (:col-id room-data)}))))
       :owner k :room-id room :canonical-workflow canonical
       :change (fn [name f & args]
                 (change! name
                          (fn [draft]
                            (let [next (apply f draft args)]
                              (cond-> next
                                (not= (:form draft) (:form next))
                                (assoc :setup-form-revision (get-in draft [:source :revision]))
                                (not= (get-in draft [:form :model]) (get-in next [:form :model]))
                                (dissoc :setup-default-model))))))
       :save save! :submit submit! :fund fund! :schedule schedule!
       :collect-output collect-output!
       :inspect-wiki-setup inspect-setup! :install-wiki-setup install-setup! :upgrade-wiki-setup upgrade-setup!
       :open-schedules (fn []
                         (binding [ec/*execution-context* ctx]
                           (when (web/current-session? session)
                             (sig/open-tab! :schedules {}
                                            {:title "Workflows" :new-tab? true
                                             :col-id (:col-id room-data)}))))
       :open-room (fn []
                    (binding [ec/*execution-context* ctx]
                      (when (web/current-session? session)
                        (sig/open-tab! :chat room-data
                                       {:title (editor/room-label room-data @sig/user-rooms) :new-tab? true
                                        :col-id (:col-id room-data)}))))
       :open-proposal (fn [proposal]
                        (binding [ec/*execution-context* ctx]
                          (when (web/current-session? session)
                            (refs/open! {:kind :proposal :id proposal}))))
       :open-leaderboard (fn []
                           (binding [ec/*execution-context* ctx]
                             (when (web/current-session? session)
                               (sig/open-tab! :leaderboard room-data
                                              {:title "Benchmark results" :new-tab? true
                                               :col-id (:col-id room-data)}))))
       :open-run (fn [job]
                   (binding [ec/*execution-context* ctx]
                     (when (web/current-session? session)
                       (sig/open-tab! :run-inspector (assoc room-data :run-id (str job))
                                      {:title "Workflow Run" :new-tab? true
                                       :col-id (:col-id room-data)}))))})))
