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
            #?(:clj [is.simm.documents.evaluation :as evaluation])
            #?(:clj [org.replikativ.spindel.engine.impl.simple :as simple])
            #?(:clj [is.simm.runtimes.context :as context])))

#?(:clj
   (defn- file-action [operation f]
     (if (#{:evaluate :repl} operation) (f)
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
     (context/with-server-context
       (let [id (when (string? room-id) (parse-uuid room-id))
             action (cond (#{:evaluate :repl} operation) :merge
                          (#{:save :move :office-save :rich-save :property-save} operation) :write
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
                                       :metadata (assoc (owned-files/metadata! conn party id room args) :protocol-version 2)
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
                                       :history (owned-files/history! conn party id room args)
                                       :history-file (owned-files/history-file! conn party id room args)
                                       :evaluate (evaluation/evaluate! room party args
                                                                       #(when-not (access/can? @(system-db/get-conn) party :merge {:room id})
                                                                          (throw (ex-info "Room evaluation permission was revoked" {:type :not-authorized}))))
                                       :repl (evaluation/evaluate-repl! room party args
                                                                        #(when-not (access/can? @(system-db/get-conn) party :merge {:room id})
                                                                           (throw (ex-info "Room evaluation permission was revoked" {:type :not-authorized}))))
                                       :download (owned-files/download! conn party id room args)
                                       :analyze (owned-files/analyze! conn party id room args)
                                       :rich (owned-files/rich! conn party id room args)
                                       :save (owned-files/save! conn party id room args)
                                       :rich-save (owned-files/rich-save! conn party id room args)
                                       :office (owned-files/office! conn party id room args)
                                       :office-save (owned-files/office-save! conn party id room args)
                                       :move (owned-files/move! conn party id room args))))]
             (assoc reply :permissions
                    {:write? (access/can? @(system-db/get-conn) party :write {:room id})
                     :evaluate? (access/can? @(system-db/get-conn) party :merge {:room id})})))))))

(defn-spin-remote prepare-memory-stream!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :watch {}))))
                  :cljs nil)))

(defn-spin-remote load-memory-metadata!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :metadata request))))
                  :cljs nil)))

(defn-spin-remote evaluate-memory-code!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :evaluate request))))
                  :cljs nil)))

(defn-spin-remote evaluate-memory-repl!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(invoke-memory party room-id-str :repl request))))
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
