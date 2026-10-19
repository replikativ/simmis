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
            #?(:clj [is.simm.documents.files :as files])
            #?(:clj [is.simm.documents.evaluation :as evaluation])
            #?(:clj [is.simm.documents.rich :as rich])
            #?(:clj [is.simm.documents.office :as office])
            #?(:clj [geschichte.query :as query])
            #?(:clj [is.simm.runtimes.context :as context])))

#?(:clj
   (defn invoke-memory [party room-id operation args]
     (when-not party (throw (ex-info "Authentication required" {:type :authentication-required})))
     (context/with-server-context
       (let [id (parse-uuid room-id)
             action (cond (#{:evaluate :repl} operation) :merge
                          (#{:save :move :office-save :rich-save} operation) :write
                          :else :read)]
         (when-not id (throw (ex-info "Invalid room identity" {:type :invalid})))
         (access/require! action {:room id})
         (let [room (rooms/live-room id)]
           (when-not room (throw (ex-info "This room is not live" {:type :room-not-live})))
           (let [authorize! #(when-not (access/can? @(system-db/get-conn) party :write {:room id})
                              (throw (ex-info "Room write permission was revoked" {:type :not-authorized})))
                 reply
                 (case operation
                   :metadata (files/metadata room args)
                   :watch (do
                            ((requiring-resolve 'is.simm.runtimes.memory-changes/register-topic!)
                             @db/server-peer party)
                            ((requiring-resolve 'is.simm.runtimes.memory-changes/watch!) id room party))
                   :folder (files/list-folder room (:folder args))
                   :search (files/search-paths room (:query args))
                   :file (files/read-file room (:path args))
                   :history (files/history room (:path args))
                   :history-file (files/read-history room (:path args) (:commit args))
                   :evaluate (evaluation/evaluate! room party args
                               #(when-not (access/can? @(system-db/get-conn) party :merge {:room id})
                                  (throw (ex-info "Room evaluation permission was revoked" {:type :not-authorized}))))
                   :repl (evaluation/evaluate-repl! room party args
                           #(when-not (access/can? @(system-db/get-conn) party :merge {:room id})
                              (throw (ex-info "Room evaluation permission was revoked" {:type :not-authorized}))))
                   :download (files/read-binary room (:path args) (:expected args))
                   :analyze (files/with-workspace room
                              (fn [conn]
                                (files/analyze (:geschichte.repo/id (query/repository @conn))
                                               (:path args) (:text args))))
                   :rich (let [{:keys [path text revision from to model]} args]
                           (when-not (= #{:path :text :revision :from :to :model} (set (keys args)))
                             (throw (ex-info "Invalid rich-edit request" {:type :invalid})))
                           (files/path! path)
                           (files/with-workspace room
                             (fn [conn]
                               (let [repo-id (:geschichte.repo/id (query/repository @conn))
                                     next (rich/apply-edit repo-id path text revision from to model)]
                                 {:text next :analysis (files/analyze repo-id path next)}))))
                   :save (files/save-file! room args {:authorize! authorize!})
                   :rich-save
                   (let [{:keys [path expected source revision edits text]} args]
                     (when-not (= #{:path :expected :source :revision :edits :text} (set (keys args)))
                       (throw (ex-info "Invalid rich document save" {:type :invalid})))
                     (files/path! path)
                     (files/with-workspace room
                       (fn [conn]
                         (let [repo-id (:geschichte.repo/id (query/repository @conn))
                               result (rich/apply-edits repo-id path source revision edits)]
                           (when-not (= result text)
                             (throw (ex-info "Rich source validation differs from the draft; use Source to inspect it" {:type :invalid})))
                           (files/save-file! room {:path path :expected expected :text result} {:authorize! authorize!})))))
                   :office (office/inspect! room (:path args) (:expected args) (:part args))
                   :office-save (office/save-part! room args {:authorize! authorize!})
                   :move (do
                           (when-not (= #{:path :target :expected} (set (keys args)))
                             (throw (ex-info "Invalid move request" {:type :invalid})))
                           (files/move-file! room (:path args) (:target args) (:expected args)
                                             {:authorize! authorize!})))]
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
  [server-id room-id-str path]
  (spin-remote server-id [room-id-str path]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :history {:path path}))))
       :cljs nil)))

(defn-spin-remote load-memory-history-file!
  [server-id room-id-str path commit-id]
  (spin-remote server-id [room-id-str path commit-id]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :history-file {:path path :commit commit-id}))))
       :cljs nil)))

(defn-spin-remote load-memory-folder!
  [server-id room-id-str folder]
  (spin-remote server-id [room-id-str folder]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :folder {:folder folder}))))
       :cljs nil)))

(defn-spin-remote load-memory-file!
  [server-id room-id-str path]
  (spin-remote server-id [room-id-str path]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :file {:path path}))))
       :cljs nil)))

(defn-spin-remote search-memory-files!
  [server-id room-id-str query-text]
  (spin-remote server-id [room-id-str query-text]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :search {:query query-text}))))
       :cljs nil)))

(defn-spin-remote analyze-memory-text!
  [server-id room-id-str path text]
  (spin-remote server-id [room-id-str path text]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :analyze {:path path :text text}))))
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
  [server-id room-id-str path expected]
  (spin-remote server-id [room-id-str path expected]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :download {:path path :expected expected}))))
       :cljs nil)))

(defn-spin-remote load-memory-office-part!
  [server-id room-id-str path expected part]
  (spin-remote server-id [room-id-str path expected part]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :office {:path path :expected expected :part part}))))
       :cljs nil)))

(defn-spin-remote save-memory-office-part!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
    #?(:clj (let [party (access/authenticated-party-id)]
              (await (blocking/blocking #(invoke-memory party room-id-str :office-save request))))
       :cljs nil)))
