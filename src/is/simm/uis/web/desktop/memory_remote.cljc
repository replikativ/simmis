(ns is.simm.uis.web.desktop.memory-remote
  "Authenticated Memory file/editor adapters. Blocking parse/store work stays
   off the FRP drain; caller identity is captured before crossing that seam."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.model.system-db :as system-db])
            #?(:clj [is.simm.model.db :as db])
            #?(:clj [is.simm.agents.room-agents :as rooms])
            #?(:clj [is.simm.ops.owned-files :as owned-files])
            #?(:clj [is.simm.ops.owned-metadata :as metadata])
            #?(:clj [is.simm.documents.files :as files])
            #?(:clj [is.simm.ops.owned-cells :as owned-cells])
            #?(:clj [is.simm.ops.file-output-adoption :as adoption])
            #?(:clj [is.simm.documents.evaluation :as evaluation])
            #?(:clj [is.simm.ops.owned-reads :as reads])
            #?(:clj [is.simm.runtimes.application-ops :as application])
            #?(:clj [is.simm.runtimes.operation-lifetime :as lifetime])
            #?(:clj [org.replikativ.spindel.engine.impl.simple :as simple])
            #?(:clj [is.simm.runtimes.context :as context])))

#?(:clj (declare file-action))

#?(:clj
   (defn- metadata-unavailable! [error]
     (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
     (case (:type (ex-data error))
       :memory/target-changed
       (throw (ex-info "The Memory repository changed; refresh before editing."
                       {:type :memory/target-changed}))
       :is.simm.ops.owned-reads/refused
       (throw (ex-info "Room permission required" {:type :not-authorized}))
       (throw (ex-info "Memory metadata is unavailable" {:type :memory/metadata-unavailable})))))

#?(:clj
   (defn prepare-metadata
     "Capture the authenticated human and original App BEFORE queuing work."
     [party room-id request]
     (when-not (uuid? party)
       (throw (ex-info "Authentication required" {:type :authentication-required})))
     (when-not (and (string? room-id) (= room-id (some-> (parse-uuid room-id) str)))
       (throw (ex-info "Invalid room identity" {:type :validation-error})))
     (files/metadata-request! request)
     (try
       (let [invoke (application/capture-operation)
             principal {:principal/kind :human :principal/id (str "party/" party)}]
         (fn []
           (try (file-action :metadata #(invoke (fn [host] (metadata/read! host principal room-id request))))
                (catch Throwable error (metadata-unavailable! error)))))
       (catch Throwable error (metadata-unavailable! error)))))

#?(:clj
   (defn prepare-evaluation
     "Capture the original application before queuing native work. The returned
      thunk starts an owned evaluation and awaits its receipt outside preflight
      admission; it never reopens ambient/replacement application handles."
     [party room-id operation request]
     (when-not (and (uuid? party) (#{:evaluate :repl :cell} operation)
                    (string? room-id) (= room-id (some-> room-id parse-uuid str)))
       (throw (ex-info "Invalid evaluation identity" {:type :validation-error})))
     (let [invoke (application/capture-operation)
           principal {:principal/kind :human :principal/id (str "party/" party)}]
       (fn []
         (let [[host selected prepared]
               (invoke (fn [host]
                         (if (= :cell operation)
                           (let [prepared (owned-cells/prepare! host principal room-id request)]
                             [host (:room-binding prepared) prepared])
                           [host (reads/resolve-host-room! host principal room-id) nil])))
               room (:room selected)
               authorize-room! #(do (lifetime/check-current!)
                                    (reads/validate-host-room! host principal room-id selected))
               authorize! #(do (authorize-room!)
                               ;; Only called under the actual Room lease,
                               ;; including AFTER waiting for the interpreter.
                               (when prepared (owned-cells/validate! host principal room-id prepared)))
               with-owner (fn [f]
                            (invoke (fn [current-host]
                                      (when-not (identical? host current-host)
                                        (throw (ex-info "Application changed" {:type :not-authorized})))
                                      (authorize-room!)
                                      (f))))
               result (if prepared
                        (assoc (evaluation/evaluate-repl!
                                room party {:text (get-in prepared [:input :code]) :syntax :clojure
                                            :namespace (:namespace request)} authorize! with-owner true)
                               :source (get-in prepared [:input :source]))
                        ((case operation :evaluate evaluation/evaluate! :repl evaluation/evaluate-repl!)
                         room party request authorize! with-owner))
               db (:control-db selected)]
           ;; Observational permissions from preflight. Execution always rechecks
           ;; current authority inside its actual worker ownership and gate.
           (assoc result :permissions {:write? (access/can? db party :write {:room (parse-uuid room-id)})
                                       :evaluate? true}))))))

#?(:clj
   (defn- file-action [operation f]
     (if (#{:evaluate :repl :cell} operation) (f)
         (try (f)
              (catch clojure.lang.ExceptionInfo error
                (let [{:keys [type reason]} (ex-data error)]
                  (if (and (#{:is.simm.ops.owned-files/refused :is.simm.ops.owned-workspace/refused} type)
                           (= :target-changed reason))
                ;; Public classification, NOT a target descriptor or authority.
                ;; Never translate denials, parser failures or entry conflicts.
                    (throw (ex-info "The Memory repository changed; refresh before editing."
                                    {:type :memory/target-changed} error))
                    (throw error))))))))

#?(:clj
   (defn invoke-memory [party room-id operation args]
     (when-not party (throw (ex-info "Authentication required" {:type :authentication-required})))
     (when (simple/on-drain-thread?)
       (throw (ex-info "Memory actions require a host worker" {:type :validation-error})))
     (cond
       (= :metadata operation) ((prepare-metadata party room-id args))
       (#{:evaluate :repl :cell} operation) ((prepare-evaluation party room-id operation args))
       :else
       (context/with-server-context
         (let [id (when (string? room-id) (parse-uuid room-id))
               action (cond (#{:evaluate :repl} operation) :merge
                            (#{:save :move :office-save :office-correct :rich-save :property-save :declaration-save} operation) :write
                            :else :read)
               conn (system-db/get-conn)]
           (when-not (and id (uuid? party)) (throw (ex-info "Invalid room identity" {:type :validation-error})))
         ;; Identity was captured BEFORE the blocking seam. Do not re-derive
         ;; ambient distributed-scope authority here, and never hydrate a Room
         ;; until this explicit actor has current read AND action permission.
           (let [db @conn]
             (when-not (and (access/can? db party :read {:room id})
                            (access/can? db party action {:room id}))
               (throw (ex-info "Room permission required" {:type :not-authorized}))))
           (let [room (rooms/live-room id)]
             (when-not room (throw (ex-info "This room is not live" {:type :room-not-live})))
             (when (and (#{:folder :search :file} operation)
                        (not (uuid? (:expected-target-stamp args))))
               (throw (ex-info "Discover the Memory repository before opening files" {:type :validation-error})))
             (let [reply
                   (file-action operation
                                (fn [] (case operation
                                         :watch (do
                                                  (when-not (= {} args)
                                                    (throw (ex-info "Invalid Memory watch" {:type :validation-error})))
                                                  ((requiring-resolve 'is.simm.runtimes.memory-changes/register-topic!)
                                                   @db/server-peer party)
                                                  ((requiring-resolve 'is.simm.runtimes.memory-changes/watch-owned!) id room party))
                                         :folder (owned-files/folder! conn party id room args)
                                         :search (owned-files/search! conn party id room args)
                                         :file (owned-files/read! conn party id room args)
                                         :properties (owned-files/properties! conn party id room args)
                                         :property-save (owned-files/property-input-save! conn party id room args)
                                         :declaration-preview (owned-files/declaration-preview! conn party id room args)
                                         :declaration-save (owned-files/declaration-save! conn party id room args)
                                         :history (owned-files/history! conn party id room args)
                                         :history-file (owned-files/history-file! conn party id room args)
                                         :download (owned-files/download! conn party id room args)
                                         :analyze (owned-files/analyze! conn party id room args)
                                         :rich (owned-files/rich! conn party id room args)
                                         :save (owned-files/save! conn party id room args)
                                         :rich-save (owned-files/rich-save! conn party id room args)
                                         :office (owned-files/office! conn party id room args)
                                         :office-save (owned-files/office-save! conn party id room args)
                                         :office-correct (owned-files/office-correct! conn party id room args)
                                         :move (owned-files/move! conn party id room args))))]
               (assoc reply :permissions
                      {:write? (access/can? @(system-db/get-conn) party :write {:room id})
                       :evaluate? (access/can? @(system-db/get-conn) party :merge {:room id})}))))))))

(defn-spin-remote prepare-memory-stream!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :watch {}))))
                  :cljs nil)))

(defn-spin-remote load-memory-metadata!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-metadata (access/authenticated-party-id) room-id-str request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote evaluate-memory-code!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-evaluation (access/authenticated-party-id) room-id-str :evaluate request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote evaluate-memory-repl!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-evaluation (access/authenticated-party-id) room-id-str :repl request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote evaluate-memory-cell!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [body (prepare-evaluation (access/authenticated-party-id) room-id-str :cell request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote load-memory-history!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :history request))))
                  :cljs nil)))

(defn-spin-remote load-memory-history-file!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :history-file request))))
                  :cljs nil)))

(defn-spin-remote load-memory-folder!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :folder request))))
                  :cljs nil)))

(defn-spin-remote load-memory-file!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :file request))))
                  :cljs nil)))

(defn-spin-remote search-memory-files!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :search request))))
                  :cljs nil)))

(defn-spin-remote load-memory-properties!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :properties request))))
                  :cljs nil)))

(defn-spin-remote save-memory-property!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :property-save request))))
                  :cljs nil)))

(defn-spin-remote preview-memory-declaration!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :declaration-preview request))))
                  :cljs nil)))

(defn-spin-remote save-memory-declaration!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :declaration-save request))))
                  :cljs nil)))

(defn-spin-remote analyze-memory-text!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :analyze request))))
                  :cljs nil)))

(defn-spin-remote apply-memory-rich-edit!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :rich request))))
                  :cljs nil)))

(defn-spin-remote save-memory-file!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :save request))))
                  :cljs nil)))

(defn-spin-remote save-memory-rich-file!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :rich-save request))))
                  :cljs nil)))

(defn-spin-remote move-memory-file!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :move request))))
                  :cljs nil)))

(defn-spin-remote download-memory-file!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :download request))))
                  :cljs nil)))

(defn-spin-remote load-memory-office-part!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :office request))))
                  :cljs nil)))

(defn-spin-remote save-memory-office-part!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :office-save request))))
                  :cljs nil)))

(defn-spin-remote correct-memory-office-text!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :office-correct request))))
                  :cljs nil)))
