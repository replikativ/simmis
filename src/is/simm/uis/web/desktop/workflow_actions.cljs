(ns is.simm.uis.web.desktop.workflow-actions
  "Browser workflow I/O; all state and callbacks retain their owning context."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.runtimes.web :as web]))

(defn state-key [room] [(:id (web/current-session)) (str room)])

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
        room (str (:room-id room-data)) k (state-key room)
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (web/current-session? session)
                      (apply swap! sig/workflow-editor-state update k f args))))
        state (fn [] (binding [ec/*execution-context* ctx] (get @sig/workflow-editor-state k)))
        change! (fn [name f & args] (apply update! update-in [:drafts name] f args))
        request!
        (fn [slot selected invoke consume]
          (let [slot (if selected [slot selected] slot)
                token (str (random-uuid))
                current? #(and (web/current-session? session)
                               (editor/current-response? (state) slot token nil))
                finish! (fn [f]
                          (binding [ec/*execution-context* ctx]
                            (when (current?) (update! f))))]
            (update! #(-> % (assoc-in [:requests slot] token)
                          (assoc-in [:loading slot] true) (update :errors dissoc slot)))
            (try
              (binding [ec/*execution-context* ctx]
                (let [s (invoke)]
                  (s (fn [reply]
                       (try
                         (finish! #(-> (consume % reply) (assoc-in [:loading slot] false)))
                         (catch :default error
                           (finish! #(-> % (assoc-in [:loading slot] false)
                                         (assoc-in [:errors slot] (rem/error-text error)))))))
                     (fn [error]
                       (finish! #(-> % (assoc-in [:loading slot] false)
                                     (assoc-in [:errors slot] (rem/error-text error))))))))
              (catch :default error
                (finish! #(-> % (assoc-in [:loading slot] false)
                              (assoc-in [:errors slot] (rem/error-text error))))))))]
    (letfn [(load-list! [offset]
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
              (update! assoc :selected name)
              (when (and (not (get-in (state) [:loading [:save name]]))
                         (not (get-in (state) [:loading [:source name]]))
                         (or reload? (nil? (get-in (state) [:drafts name :source]))))
                (request! :source name #(remote/load-workflow-source! web/server-id room name)
                          (fn [s reply] (-> s (assoc :permissions (:permissions reply))
                                            (assoc-in [:drafts name]
                                                      (cond-> (editor/reloaded (get-in s [:drafts name]) reply)
                                                        (and create? (empty? (:files reply)))
                                                        (assoc :files editor/scaffold :path "workflow.edn"))))))))
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
                                  (update-in [:drafts name] editor/saved sent reply)))))))
            (submit! [name retry?]
              (let [draft (get-in (state) [:drafts name])
                    pending (:submission draft)]
                (try
                  (when (or (get-in (state) [:loading [:submit name]])
                            (and (not retry?) pending (nil? (:reply pending))))
                    (throw (js/Error. "Retry the existing unresolved request instead of creating another")))
                  (let [payload (if retry? (:payload pending) (editor/submission draft (str (random-uuid))))
                        mode (if retry? (:mode pending) (get-in draft [:form :mode]))
                        backend (if retry? (get pending :backend "hosted") (get-in draft [:form :backend] "hosted"))]
                    (when-not payload (throw (js/Error. "There is no request to retry")))
                    (when-not (or retry?
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
                    (change! name assoc :submission (if retry? pending {:payload payload :mode mode :backend backend}))
                    (request! :submit name
                              #((submitter backend mode) web/server-id room payload)
                              (fn [s reply] (assoc-in s [:drafts name :submission :reply] reply))))
                  (catch :default error (update! assoc-in [:errors :submit] (rem/error-text error))))))
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
                                                (assoc-in [:drafts name :fund-reply] reply))))))
                (catch :default error (update! assoc-in [:errors :fund] (rem/error-text error)))))]
      {:refresh #(load-list! 0) :more #(load-list! (get-in (state) [:list :next-offset]))
       :workers #(load-workers! nil) :more-workers #(load-workers! (get-in (state) [:workers :next-after]))
       :select #(select! % false) :reload #(select! % true)
       :new-name #(update! assoc :new-name %)
       :create (fn []
                 (let [name (:new-name (state))]
                   (if (and (string? name) (re-matches #"[a-z0-9][a-z0-9-]{0,63}" name))
                     (select! name (nil? (get-in (state) [:drafts name])) true)
                     (update! assoc-in [:errors :source] "Use a lowercase workflow name with letters, digits and dashes (max64)."))))
       :change change! :save save! :submit submit! :fund fund!
       :open-run (fn [job]
                   (binding [ec/*execution-context* ctx]
                     (when (web/current-session? session)
                       (sig/open-tab! :run-inspector (assoc room-data :run-id (str job))
                                      {:title "Workflow Run" :new-tab? true
                                       :col-id (:col-id room-data)}))))})))
