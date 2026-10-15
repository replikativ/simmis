(ns is.simm.uis.web.desktop.sandbox-remote
  "Sandbox state inspection and code execution — spin-remote interface."
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            #?(:clj [is.simm.agents.room-agents :as room-agents])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.model.system-db :as system-db])
            #?(:clj [is.simm.model.room-databases :as room-dbs])
            #?(:clj [is.simm.model.knowledge-bases :as kbs])
            #?(:clj [dvergr.sandbox :as sandbox])
            #?(:clj [datahike.api :as d])
            #?(:clj [clojure.string :as str])
            #?(:clj [taoensso.telemere :as log])))

(def server-id #uuid "05a06e85-e7ca-4213-9fe5-04ae511e50a0")

;; =============================================================================
;; Server-side helpers
;; =============================================================================

#?(:clj
   (defn- parse-uuid [v]
     (cond (uuid? v) v
           (string? v) (java.util.UUID/fromString v)
           :else nil)))

#?(:clj
   (defn- namespace-vars [sci-ctx]
     (try
       (let [vars-map (sandbox/ns-publics-in-sci sci-ctx 'user)]
         (->> vars-map
              (map (fn [[sym v]]
                     (let [val (try @v (catch Exception _ ::unreadable))
                           type-kw (cond (fn? val) :function
                                         (= val ::unreadable) :unreadable
                                         :else :value)
                           preview (try
                                     (let [s (pr-str (if (= val ::unreadable) "?" val))]
                                       (if (> (count s) 100) (str (subs s 0 97) "…") s))
                                     (catch Exception _ "?"))]
                       {:name (str sym) :type type-kw :preview preview})))
              (sort-by :name)
              vec))
       (catch Exception e
         (log/log! {:level :warn :id ::namespace-vars-error
                    :msg "Failed to read namespace vars" :data {:error (.getMessage e)}})
         []))))

#?(:clj
   (defn- definition-form? [code]
     (let [trimmed (str/triml code)]
       (re-find #"^\((?:defn|def |defmacro|defonce|defprotocol|defrecord|deftype|require)\b"
                trimmed))))

#?(:clj
   (defn- eval-history [room-conn room-uuid]
     (try
       (let [entries (d/q '[:find ?code ?result ?ts
                             :keys S.EvalEntry/code S.EvalEntry/result S.EvalEntry/evaluated-at
                             :in $ ?room-uuid
                             :where
                             [?room :entity/uuid ?room-uuid]
                             [?e :S.EvalEntry/room ?room]
                             [?e :S.EvalEntry/code ?code]
                             [?e :S.EvalEntry/result ?result]
                             [?e :S.EvalEntry/success? true]
                             [?e :S.EvalEntry/evaluated-at ?ts]]
                           @room-conn room-uuid)
             defs (->> entries
                       (filter #(definition-form? (:S.EvalEntry/code %)))
                       (sort-by :S.EvalEntry/evaluated-at))]
         (->> defs
              (group-by (fn [e]
                          (second (re-find #"\(\w+\s+(\S+)" (:S.EvalEntry/code e)))))
              (map (fn [[name entries]]
                     (let [latest (last entries)]
                       {:name name
                        :code (:S.EvalEntry/code latest)
                        :result (:S.EvalEntry/result latest)
                        :evaluated-at (:S.EvalEntry/evaluated-at latest)})))
              (sort-by :name)
              vec))
       (catch Exception e
         (log/log! {:level :warn :id ::eval-history-error
                    :msg "Failed to query eval history" :data {:error (.getMessage e)}})
         []))))

#?(:clj
   (defn- git-run [dir & args]
     (try
       (let [pb (ProcessBuilder. (into-array String (cons "git" args)))
             _ (.directory pb (java.io.File. dir))
             proc (.start pb)
             out (slurp (.getInputStream proc))
             _ (.waitFor proc)]
         (str/trim out))
       (catch Exception _ nil))))

#?(:clj
   (defn- repo-info [cwd]
     (when-let [root (git-run cwd "rev-parse" "--show-toplevel")]
       (when (seq root)
         (let [branch    (or (git-run root "rev-parse" "--abbrev-ref" "HEAD") "unknown")
               status    (git-run root "status" "--porcelain")
               lines     (when status (str/split-lines status))
               modified  (count (filter #(re-find #"^.M|^M" %) (or lines [])))
               untracked (count (filter #(str/starts-with? % "??") (or lines [])))
               commits   (git-run root "log" "--oneline" "-5")]
           {:path root
            :name (last (str/split root #"/"))
            :branch branch
            :modified modified
            :untracked untracked
            :recent-commits (when commits
                              (->> (str/split-lines commits)
                                   (map #(let [[sha & msg] (str/split % #" " 2)]
                                           {:sha sha :msg (str/join " " msg)}))
                                   vec))})))))

#?(:clj
   (defn- db-info [room-uuid room-conn]
     (try
       (let [room-count (count (d/q '[:find ?e :where [?e :entity/uuid _]] @room-conn))
             kb-conns (try
                        (->> (kbs/get-room-kbs room-uuid)
                             (keep (fn [kb]
                                     (when-let [conn (kbs/connect-kb-database (:kb/db-scope kb))]
                                       {:name (:kb/name kb)
                                        :entity-count (count (d/q '[:find ?e :where [?e :entity/uuid _]] @conn))}))))
                        (catch Exception _ []))]
         (into [{:name "room-db" :type :room :entity-count room-count}]
               (map #(assoc % :type :kb) kb-conns)))
       (catch Exception _ []))))

;; =============================================================================
;; Server functions (called by spin-remote on JVM side)
;; =============================================================================

#?(:clj
   (defn- find-room-context [room-uuid]
     (room-agents/room-context-state room-uuid)))

#?(:clj
   (defn- may-eval? [viewer room-uuid]
     (boolean (and viewer room-uuid
                   (access/may-eval-in-room? (some-> (system-db/get-conn) deref)
                                             viewer room-uuid)))))

#?(:clj
   (defn get-room-state-server
     "The sandbox panel's state. `viewer` (the authenticated party) decides
      `:can-eval?`, so the panel shows the eval box only to who may use it."
     [room-uuid-str viewer]
     (let [uuid     (parse-uuid room-uuid-str)
           state    (find-room-context uuid)
           sci-ctx  (:sci-ctx state)
           db-scope (when uuid (room-dbs/get-room-db-scope uuid))
           room-conn (when db-scope (room-dbs/connect-room-database db-scope))
           cwd      (System/getProperty "user.dir")]
       {:can-eval?      (may-eval? viewer uuid)
        :namespace-vars (if sci-ctx (namespace-vars sci-ctx) [])
        :eval-history   (if room-conn (eval-history room-conn uuid) [])
        :databases      (if room-conn (db-info uuid room-conn) [])
        :repos          (keep identity [(repo-info cwd)])})))

#?(:clj
   (defn eval-in-room-server
     "Evaluate `code` in the room's sandbox and record it under `author`, the
      AUTHENTICATED party (`access/authenticated-party-id`, captured by the
      caller). The RPC used to take the author from the client, so a member's
      code was recorded as whoever they named. Who may call this at all is the
      policy row's: `:merge` on the room, the owner."
     [room-uuid-str code author]
     (let [uuid  (parse-uuid room-uuid-str)
           state (find-room-context uuid)]
       (if-let [sci-ctx (:sci-ctx state)]
         (let [result     (sandbox/eval-code sci-ctx code)
               success?   (:success result)
               result-str (if success?
                            (str "=> " (pr-str (:value result))
                                 (when (seq (:stdout result))
                                   (str "\n" (:stdout result))))
                            (str "Error: " (:message (:error result))))
               db-scope   (room-dbs/get-room-db-scope uuid)
               room-conn  (when db-scope (room-dbs/connect-room-database db-scope))]
           (when room-conn
             (try
               (let [now (java.util.Date.)]
                 (d/transact room-conn
                   [{:entity/uuid      (random-uuid)
                     :entity/created-at now
                     :S.EvalEntry/room  [:entity/uuid uuid]
                     :S.EvalEntry/agent [:entity/uuid author]
                     :S.EvalEntry/code  code
                     :S.EvalEntry/result (let [s result-str]
                                           (if (> (count s) 2000) (str (subs s 0 2000) "\n…") s))
                     :S.EvalEntry/success? success?
                     :S.EvalEntry/evaluated-at now}]))
               (catch Exception e
                 (log/log! {:level :warn :id ::eval-persist-failed
                            :msg "Failed to persist eval entry"
                            :data {:error (.getMessage e)}}))))
           {:success success? :result result-str})
         {:success false
          :result "No sandbox context for room — send a message first to initialize the agent."}))))

;; =============================================================================
;; Spin-remote interface
;; =============================================================================

(defn-spin-remote get-room-state!
  [server-id room-uuid-str]
  (spin-remote server-id [room-uuid-str]
    (let [ruid (identity room-uuid-str)]
      #?(:clj  (get-room-state-server ruid (access/authenticated-party-id))
         :cljs nil))))

;; No author argument: the author is the connection's principal, read here,
;; synchronously, while it is bound.
(defn-spin-remote eval-in-room!
  [server-id room-uuid-str code]
  (spin-remote server-id [room-uuid-str code]
    (let [ruid (identity room-uuid-str)
          c    (identity code)]
      #?(:clj  (eval-in-room-server ruid c (access/authenticated-party-id))
         :cljs nil))))

(defn-spin-remote can-eval-in-room!
  [server-id room-uuid-str]
  (spin-remote server-id [room-uuid-str]
    (let [ruid (identity room-uuid-str)]
      #?(:clj  (may-eval? (access/authenticated-party-id) (parse-uuid ruid))
         :cljs nil))))

;; Convenience wrappers used by client UI (server-id pre-applied)
(defn get-room-state-remote! [room-uuid]
  (get-room-state! server-id (str room-uuid)))

(defn eval-in-room-remote! [room-uuid code]
  (eval-in-room! server-id (str room-uuid) code))

(defn can-eval-in-room-remote! [room-uuid]
  (can-eval-in-room! server-id (str room-uuid)))
