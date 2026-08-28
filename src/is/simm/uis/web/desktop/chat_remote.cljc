(ns is.simm.uis.web.desktop.chat-remote
  "Spin-remote functions for room management and messaging."
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            #?(:clj [is.simm.model.rooms :as rooms])
            #?(:clj [is.simm.model.parties :as parties])
            #?(:clj [is.simm.model.db :as db])
            #?(:clj [is.simm.model.room-databases :as room-dbs])
            #?(:clj [is.simm.model.knowledge-bases :as kbs])
            #?(:clj [is.simm.model.drives :as drives])
            #?(:clj [is.simm.model.system-db :as system-db])
            #?(:clj [is.simm.agents.room-agents :as room-agents])
            #?(:clj [is.simm.agents.templates :as templates])
            #?(:clj [dvergr.chat.context :as chat-ctx])
            #?(:clj [dvergr.chat.accounting :as acct])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.model.message-notify-broadcast :as mnb])
            #?(:clj [is.simm.model.user-rooms-broadcast :as urb])
            #?(:clj [datahike.api :as d])
            [clojure.string :as str]))

;; =============================================================================
;; Rooms + agents + KBs listing (initial load)
;; =============================================================================

(defn assignment-summary
  "The room-roster projection of one durable dvergr assignment.

   Actor ids are globally stable `:party/<uuid>` keywords.  The client already
   uses UUID strings for party identity, so expose that UUID plus only the two
   room-local policy fields needed by the everyday room UI.  Non-party actors
   remain valid dvergr actors, but are not Simmis roster entries and therefore
   do not get fabricated party identities here."
  [assignment]
  (let [actor-id (:assignment/actor-id assignment)]
    (when (and (keyword? actor-id) (= "party" (namespace actor-id)))
      {:actor-id (name actor-id)
       :role (:assignment/role assignment)
       :response-policy (:assignment/response-policy assignment)})))

(defn agent-assignment-summary
  "Project an agent's effective room policy without materializing a legacy
   assignment. A durable assignment wins; `auto-respond?` supplies the same
   one-time fallback used by dispatch."
  [agent assignment]
  {:actor-id (str (:party/id agent))
   :role (or (:assignment/role assignment) :specialist)
   :response-policy (or (:assignment/response-policy assignment)
                        (if (:party/auto-respond? agent) :always :manual))})

#?(:clj
   (defn- room-assignment-summaries
     "Roster policies for every agent member, including pre-assignment rooms.

      Dispatch materializes the same fallback on first use.  Loading navigation
      must remain a read, however, so project the fallback here rather than
      creating assignments merely because a client opened the application."
     [room]
     (let [durable (when-let [slug (:room/slug room)]
                     (into {} (map (juxt :assignment/actor-id identity))
                           (room-agents/dvergr-room-assignments slug)))]
       (->> (:room/parties room)
            (keep parties/get-party)
            (filter #(= :agent (:party/type %)))
            (mapv (fn [agent]
                    (let [actor-id (room-agents/party->actor-kw agent)
                          assignment (get durable actor-id)]
                      (agent-assignment-summary agent assignment))))))))

(defn-spin-remote load-rooms!
  [server-id party-id-str]
  (spin-remote server-id [party-id-str]
    (let [pid (identity party-id-str)]
      #?(:clj (let [party-id  (java.util.UUID/fromString pid)
                    party-kbs (kbs/get-party-kbs party-id)
                    party-rooms (rooms/get-party-rooms party-id)
                    ;; Derived contact set: explicit :party/contacts + everyone in shared rooms
                    ;; (excluding self). Agent entries carry their primary room for open-in-tab.
                    explicit-contacts (parties/get-contacts party-id)
                    room-party-map (reduce
                                     (fn [acc r]
                                       (reduce (fn [a p]
                                                 (if (and (not= p party-id)
                                                          (not (contains? a p)))
                                                   (assoc a p {:room-id (:room/id r)
                                                               :room-name (:room/name r)})
                                                   a))
                                               acc
                                               (:room/parties r)))
                                     {}
                                     party-rooms)
                    contact-ids (into (set (keys room-party-map))
                                      (map :party/id explicit-contacts))
                    explicit-ids (into #{} (map :party/id explicit-contacts))
                    contacts (->> contact-ids
                                  (keep (fn [cid]
                                          (when-let [p (parties/get-party cid)]
                                            (let [room-info (get room-party-map cid)]
                                              (cond-> {:id           (str (:party/id p))
                                                       :type         (:party/type p)
                                                       :handle       (:party/handle p)
                                                       :display-name (:party/display-name p)
                                                       :avatar       (:party/avatar p)
                                                       :explicit?    (contains? explicit-ids cid)}
                                                room-info (assoc :room-id (str (:room-id room-info))
                                                                 :room-name (:room-name room-info))
                                                (= :agent (:party/type p))
                                                (assoc :model (:party/model p)
                                                       :provider (:party/provider p)
                                                       :auto-respond? (boolean (:party/auto-respond? p))))))))
                                  (sort-by (juxt #(case (:type %) :human 0 :agent 1 2)
                                                 :display-name))
                                  vec)]
                {:rooms (mapv (fn [r]
                                (-> r
                                    (assoc :room/assignments
                                           (room-assignment-summaries r))
                                    (update :room/created #(when % (str %)))
                                    (update :room/content-db-scope #(when % (str %)))
                                    ;; Stringify the room's attached KB
                                    ;; UUIDs so the click handler can
                                    ;; resolve [[Page]] references in
                                    ;; chat messages against the right
                                    ;; KBs without a server roundtrip.
                                    (update :room/knowledge-bases
                                            (fn [kbs] (when (seq kbs) (mapv str kbs))))
                                    ;; Room participants (uuid strings) — the
                                    ;; context footer lists them per room;
                                    ;; `contacts` doubles as the name directory.
                                    (update :room/parties
                                            (fn [ps] (mapv str ps)))))
                              party-rooms)
                 :contacts contacts
                 ;; Everyone in the tenant, name/handle/avatar only — the
                 ;; @-mention DIRECTORY.
                 ;;
                 ;; `:contacts` is explicit contacts plus people in shared
                 ;; rooms, which is right for "who do I talk to" and too narrow
                 ;; for "who can I name". Mentioning a colleague you share no
                 ;; room with is exactly how you pull them into a wiki page, and
                 ;; the S/Person projection this replaces made everyone
                 ;; mentionable. Losing that silently inside a refactor would be
                 ;; a regression, so it moves here rather than disappearing.
                 ;;
                 ;; Deliberately thinner than `:contacts`: no model, no
                 ;; provider, no room. Just enough to render and insert a
                 ;; mention. Tenant-wide today because the workspace is one
                 ;; tenant; when it is not, this is where `can?` filters.
                 :directory (->> (parties/list-parties)
                                 (mapv (fn [p]
                                         {:id (str (:party/id p))
                                          :type (:party/type p)
                                          :handle (:party/handle p)
                                          :display-name (:party/display-name p)
                                          :avatar (:party/avatar p)})))
                 :knowledge-bases
                 (->> party-kbs
                      (mapv (fn [kb]
                              ;; Page titles live in the KB store, not in the
                              ;; roster. The client opens that store when this
                              ;; wiki is expanded or viewed and derives the
                              ;; folder tree reactively from its local DB.
                              (-> kb
                                  (update :kb/created #(when % (str %)))
                                  (update :kb/db-scope str)))))
                 ;; Drives reachable from the user's rooms (the sidebar
                 ;; Files section). Room-scoped: clicking opens that
                 ;; room's :files tab.
                 :drives
                 (let [attached (->> party-rooms
                                     (mapcat (fn [r]
                                               (map (fn [drv] (assoc drv :room-id (:room/id r)
                                                                     :room-name (:room/name r)))
                                                    (drives/get-room-drives (:room/id r)))))
                                     (map (fn [drv]
                                            {:id (str (:drive/id drv))
                                             :name (:drive/name drv)
                                             :room-id (str (:room-id drv))
                                             :room-name (:room-name drv)}))
                                     distinct vec)
                       attached-ids (into #{} (map :id) attached)
                       ;; owned but not yet attached anywhere — visible so
                       ;; the user can find them; attach happens in room
                       ;; settings (create and attach stay separate).
                       own (->> (drives/list-drives party-id)
                                (map (fn [drv] {:id (str (:drive/id drv))
                                                :name (:drive/name drv)}))
                                (remove #(attached-ids (:id %)))
                                vec)]
                   (->> (concat attached own) (sort-by :name) vec))})
         :cljs nil))))

;; Register one authorized room or KB store immediately before a client opens
;; it. Navigation stays metadata-only; the selected store pays its own setup
;; cost and is guaranteed to have a konserve-sync topic before Datahike
;; subscribes.
(defn-spin-remote prepare-store!
  [server-id db-scope-str]
  (spin-remote server-id [db-scope-str]
    #?(:clj
       (let [scope (java.util.UUID/fromString db-scope-str)
             conn (system-db/get-conn)
             server-peer (when-let [get-server (resolve 'is.simm.runtimes.web/get-server)]
                           (get-server))
             already-registered? (and server-peer
                                      (contains? (get-in @server-peer [:pubsub :topics]) scope))
             room? (and conn
                        (d/q '[:find ?r .
                               :in $ ?scope
                               :where [?r :room/content-db-scope ?scope]]
                             @conn scope))
             kb? (and conn
                      (d/q '[:find ?kb .
                             :in $ ?scope
                             :where [?kb :kb/db-scope ?scope]]
                           @conn scope))]
         (when-not server-peer
           (throw (ex-info "Web server peer is not available"
                           {:db-scope scope})))
         (cond
           room? (room-dbs/register-room-for-sync! scope server-peer)
           kb? (kbs/register-kb-for-sync! scope server-peer)
           :else (throw (ex-info "Unknown room or KB store"
                                 {:db-scope scope})))
         ;; Startup no longer opens every durable store merely to clean stale
         ;; overlay/fork branches. Schedule that maintenance after first
         ;; selection; it must not hold up the client's handshake.
         (when-not already-registered?
           (future
             (try
               ((requiring-resolve 'is.simm.runtimes.branching/gc-internal-branches!) scope)
               (catch Exception _ nil))))
         {:db-scope db-scope-str :ready? true})
       :cljs nil)))

(defn-spin-remote create-room!
  [server-id creator-id-str room-name member-id-strs]
  (spin-remote server-id [creator-id-str room-name member-id-strs]
    (let [cid (identity creator-id-str)
          rn (identity room-name)
          mids (identity member-id-strs)]
      #?(:clj (let [creator-id (java.util.UUID/fromString cid)
                    member-ids (mapv #(java.util.UUID/fromString %) mids)
                    room (rooms/create-room! creator-id rn :group member-ids)]
                {:room/id (str (:room/id room))
                 :room/name (:room/name room)
                 :room/type (:room/type room)})
         :cljs nil))))

;; List all parties for the invite picker.
;; opts: {:type :human | :agent} (server filters). :all returns everyone.
(defn-spin-remote list-parties!
  [server-id party-type-kw]
  (spin-remote server-id [party-type-kw]
    (let [t (identity party-type-kw)]
      #?(:clj (parties/list-parties (when (keyword? t) t))
         :cljs nil))))

;; =============================================================================
;; Room initialization + messages
;; =============================================================================

#?(:clj
   (defn ensure-room-chatroom! [room-conn room-id room-name]
     (when-not (d/q '[:find ?e . :in $ ?uuid :where [?e :entity/uuid ?uuid]]
                    @room-conn room-id)
       (d/transact room-conn
         [{:entity/uuid room-id
           :entity/created-at (java.util.Date.)
           :entity/name (str "S.ChatRoom/" room-name)
           :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"]
           :S.ChatRoom/name room-name}]))))

#?(:clj
   (defn- ensure-party-projection! [room-conn party-id]
     "Cache the party's display info as a S.User projection in the room DB."
     (when-not (d/q '[:find ?e . :in $ ?uuid :where [?e :entity/uuid ?uuid]]
                    @room-conn party-id)
       (when-let [party (parties/get-party party-id)]
         (d/transact room-conn
           [(cond-> {:entity/uuid party-id
                     :entity/created-at (java.util.Date.)
                     :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]
                     :S.User/display-name (or (:party/display-name party) "Unknown")}
              (:party/handle party) (assoc :S.User/handle (:party/handle party))
              (:party/email party)  (assoc :S.User/email (:party/email party))
              (= :agent (:party/type party)) (assoc :S.User/is-ai true))])))))

(defn-spin-remote ensure-room!
  [server-id room-id-str party-id-str]
  (spin-remote server-id [room-id-str party-id-str]
    (let [rid (identity room-id-str)
          pid (identity party-id-str)]
      #?(:clj
         (let [room-uuid (java.util.UUID/fromString rid)
               author-uuid (java.util.UUID/fromString pid)
               room-info (rooms/get-room room-uuid)
               db-scope (:room/content-db-scope room-info)
               room-conn (when db-scope (room-dbs/connect-room-database db-scope))]
           (when (and room-info room-conn)
             (ensure-room-chatroom! room-conn room-uuid (:room/name room-info))
             (ensure-party-projection! room-conn author-uuid))
           {:status :ok :room-type (:room/type room-info)})
         :cljs nil))))

;; =============================================================================
;; Schedules (agenda view — slice 1 of the calendar)
;; =============================================================================

;; All active schedules across the caller's rooms, enriched for display:
;; room name, agent display name, stringified ids/dates (epoch ms).
;; (defn-spin-remote takes no docstring — args vector must follow the name.)
#?(:clj
   (defn- workflow-topology-mermaid
     "A katzen topology diagram (mermaid string) for a room's workflows: one node
      per schedule, each feeding the room's knowledge base. Cadence in the label;
      a code task gets the :run shape, a prompt task :program. Right-altitude by
      design — one node per WORKFLOW, never per line of code (fn->diagram on the
      raw code explodes to ~90 nodes and drowns you, which is the whole problem)."
     [schedules kb-names]
     (when (seq schedules)
       (let [cadence (fn [s] (cond (:every s)       (str (name (:every s)) " " (:at s))
                                   (:interval-ms s) (str (long (/ (:interval-ms s) 3600000)) "h")
                                   :else            "on demand"))
             clip    (fn [s n] (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) "…") s)))
             ;; Name the actual target(s) — the room's attached KB(s) — so the
             ;; diagram is at least a little specific about WHERE it writes.
             kb-label (cond
                        (empty? kb-names)      "Knowledge Base"
                        (= 1 (count kb-names)) (first kb-names)
                        :else                  (str (count kb-names) " KBs: "
                                                    (str/join ", " kb-names)))
             boxes   (conj (vec (map-indexed
                                  (fn [i s] {:id    (str "w" i)
                                             :label (str (clip (:description s) 46) "  (" (cadence s) ")")
                                             :kind  (if (:code s) :run :program)
                                             :out   (str "w" i "o")})
                                  schedules))
                           {:id "kb" :label kb-label :out "kbo"})
             wires   (vec (map-indexed (fn [i _] {:from (str "w" i "o") :to "kbo"}) schedules))]
         ((requiring-resolve 'katzen.diagram/->mermaid)
          {:name "workflows" :inputs [] :boxes boxes :wires wires :outputs ["kbo"]})))))

(defn-spin-remote load-schedules!
  [server-id]
  (spin-remote server-id []
    #?(:clj
       (let [party-id (access/authenticated-party-id)
             party-rooms (rooms/get-party-rooms party-id)
             my-slugs (into #{} (map :room/slug) party-rooms)
             slug->name (into {} (map (juxt :room/slug :room/name)) party-rooms)
             slug->uuid (into {} (map (juxt :room/slug :room/id)) party-rooms)
             agent-name (fn [agent-kw]
                          (or (some-> agent-kw
                                      room-agents/actor-kw->party-uuid
                                      parties/get-party
                                      :party/display-name)
                              (some-> agent-kw name)))
             mine (->> ((requiring-resolve 'dvergr.scheduler.core/list-all-schedules))
                       (filter #(contains? my-slugs (:room %))))
             ;; Agent-authored per-workflow shapes (design B) — schedule uuid →
             ;; mermaid, from the system DB. The client renders these drill-downs
             ;; under the topology; a schedule with no shape just shows in the map.
             shapes (let [sys @(system-db/get-conn)]
                      (into {} (for [s mine
                                     :let [m (d/q '[:find ?m . :in $ ?id :where
                                                    [?e :workflow.shape/id ?id]
                                                    [?e :workflow.shape/mermaid ?m]]
                                                  sys (:id s))]
                                     :when m]
                                 [(str (:id s)) m])))]
         {:schedules
          (mapv (fn [s]
                  (-> s
                      (update :id str)
                      (update :agent-id #(when % (str %)))
                      (assoc :agent-name (agent-name (:agent-id s)))
                      (assoc :room-name (get slug->name (:room s) (:room s)))
                      (update :next-fire #(some-> ^java.util.Date % .getTime))
                      (update :last-run #(some-> ^java.util.Date % .getTime))))
                mine)
          ;; Per-room workflow topology diagrams (katzen → mermaid), keyed by
          ;; room name; the client renders them with mermaid.js.
          :diagrams (into {} (for [[slug ss] (group-by :room mine)]
                               (let [kb-names (some->> (get slug->uuid slug)
                                                       kbs/get-room-kbs
                                                       (mapv :kb/name)
                                                       (remove str/blank?)
                                                       vec)]
                                 [(get slug->name slug slug)
                                  (workflow-topology-mermaid ss kb-names)])))
          :shapes shapes})
       :cljs nil)))

;; =============================================================================
;; Unread badges — durable read cursors (mentions-notifications design 3B)
;; =============================================================================

(defn-spin-remote load-unread-counts!
  [server-id]
  (spin-remote server-id []
    #?(:clj (let [party-id (access/authenticated-party-id)]
              (mnb/unread-counts-for-party party-id))
       :cljs nil)))

(defn-spin-remote mark-read!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    #?(:clj (let [party-id (access/authenticated-party-id)]
              (mnb/mark-read! party-id (java.util.UUID/fromString room-id-str))
              true)
       :cljs nil)))

(defn-spin-remote load-notify-prefs!
  [server-id]
  (spin-remote server-id []
    #?(:clj (let [party-id (access/authenticated-party-id)]
              (mnb/notify-prefs-for-party party-id))
       :cljs nil)))

(defn-spin-remote set-notify-pref!
  [server-id room-id-str level-kw]
  (spin-remote server-id [room-id-str level-kw]
    #?(:clj (let [party-id (access/authenticated-party-id)]
              (mnb/set-notify-pref! party-id (java.util.UUID/fromString room-id-str) level-kw)
              true)
       :cljs nil)))

;; =============================================================================
;; Room details (settings page)
;; =============================================================================

(defn-spin-remote load-room-details!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    (let [rid (identity room-id-str)]
      #?(:clj
         (let [room-uuid (java.util.UUID/fromString rid)
               room (rooms/get-room room-uuid)
               humans (rooms/get-room-humans room-uuid)
               raw-agents (rooms/get-room-agents room-uuid)
               assignments (when-let [slug (:room/slug room)]
                             (into {}
                                   (map (juxt :assignment/actor-id identity))
                                   (room-agents/dvergr-room-assignments slug)))
               agents (mapv (fn [agent]
                              (let [assignment (get assignments
                                                    (room-agents/party->actor-kw agent))]
                                (merge agent
                                       {:assignment/role
                                        (or (:assignment/role assignment) :specialist)
                                        :assignment/response-policy
                                        (or (:assignment/response-policy assignment)
                                            (if (:party/auto-respond? agent)
                                              :always
                                              :manual))
                                        :assignment/config
                                        (or (:assignment/config assignment) {})})))
                            raw-agents)
               all-humans (parties/list-parties :human)
               room-kbs (or (kbs/get-room-kbs room-uuid) [])
               party-kbs (->> (:room/parties room)
                              (mapcat #(kbs/get-party-kbs %))
                              (distinct)
                              vec)]
           {:room (when room
                    (-> room
                        (update :room/created #(when % (str %)))
                        (assoc :room/budget-dollars (rooms/get-room-budget-dollars room-uuid))
                        (dissoc :room/parties)))
            :humans humans
            :agents agents
            :all-humans all-humans
            :knowledge-bases (mapv (fn [kb]
                                     (-> kb
                                         (update :kb/created #(when % (str %)))
                                         (dissoc :db/id)))
                                   room-kbs)
            :available-kbs (mapv (fn [kb]
                                   (-> kb
                                       (update :kb/created #(when % (str %)))
                                       (dissoc :db/id)))
                                 party-kbs)
            :drives (mapv (fn [drv]
                            (-> drv
                                (update :drive/created #(when % (str %)))
                                (dissoc :db/id)))
                          (or (drives/get-room-drives room-uuid) []))
            :available-drives (->> (:room/parties room)
                                   (mapcat #(drives/list-drives %))
                                   (distinct)
                                   (mapv (fn [drv]
                                           (-> drv
                                               (update :drive/created #(when % (str %)))
                                               (dissoc :db/id)))))})
         :cljs nil))))

;; =============================================================================
;; Video calls (jitsi) — meeting token for the :video tab embed
;; =============================================================================

(defn-spin-remote mint-video-token!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    (let [rid (identity room-id-str)]
      #?(:clj
         (let [party-id (access/authenticated-party-id)
               party    (parties/get-party party-id)
               room     (rooms/get-room (java.util.UUID/fromString rid))]
           (if (and party room)
             {:status :ok
              :url    @(requiring-resolve 'is.simm.runtimes.auth-config/jitsi-url)
              :room   rid
              :jwt    ((requiring-resolve 'is.simm.runtimes.auth-config/mint-jitsi-token)
                       party rid)}
             {:status :error :reason (if party :room-not-found :not-authenticated)}))
         :cljs nil))))

;; Room static site (dvergr.web.apps): a room serves its workspace `app/`
;; directory at /apps/<slug>/. The client needs the SLUG (it only holds the
;; room uuid) to build that URL, and :has-app? to decide whether to surface the
;; "Open app" affordance. Read-scoped on the room (membership-gated).
(defn-spin-remote room-app-status!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    (let [rid (identity room-id-str)]
      #?(:clj
         (let [party-id (access/authenticated-party-id)
               room     (rooms/get-room (java.util.UUID/fromString rid))]
           (if (and party-id room (:room/slug room))
             {:status :ok
              :slug     (:room/slug room)
              :has-app? (boolean ((requiring-resolve 'dvergr.web.apps/app-exists?)
                                  (:room/id room)))}
             {:status :error :reason (if party-id :room-not-found :not-authenticated)}))
         :cljs nil))))

;; Screen-share GRANTS (doc/archive/screen-capture-scoping.md) — control-plane, so RPCs,
;; not HTTP routes. The share button toggles a room's time-boxed window onto the
;; caller's own capture stream. Membership-gated: you can only grant a room you
;; belong to. Heartbeats keep the window fresh so a dead client fails safe.
(defn-spin-remote open-screen-grant!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    #?(:clj
       (let [party-id (access/authenticated-party-id)
             room-uuid (try (java.util.UUID/fromString room-id-str) (catch Throwable _ nil))
             room (when room-uuid ((requiring-resolve 'is.simm.model.rooms/get-room) room-uuid))]
         (cond
           (nil? party-id) {:status :error :error :authentication-required}
           (nil? room) {:status :error :error :unknown-room}
           (not (contains? (set (:room/parties room)) party-id))
           {:status :error :error :not-a-room-member}
           :else (do ((requiring-resolve 'is.simm.model.screen-grants/open-grant-with-personal!) party-id room-uuid)
                     {:status :ok})))
       :cljs nil)))

(defn-spin-remote close-screen-grant!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    #?(:clj
       (let [party-id (access/authenticated-party-id)
             room-uuid (try (java.util.UUID/fromString room-id-str) (catch Throwable _ nil))]
         (if (and party-id room-uuid)
           (do ((requiring-resolve 'is.simm.model.screen-grants/close-grant-with-personal!) party-id room-uuid)
               {:status :ok})
           {:status :error :error :authentication-required}))
       :cljs nil)))

(defn-spin-remote screen-grant-heartbeat!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
    #?(:clj
       (let [party-id (access/authenticated-party-id)
             room-uuid (try (java.util.UUID/fromString room-id-str) (catch Throwable _ nil))]
         (when (and party-id room-uuid)
           ((requiring-resolve 'is.simm.model.screen-grants/heartbeat-with-personal!) party-id room-uuid))
         {:status :ok})
       :cljs nil)))

;; Screens gallery — the caller's OWN screen stream (doc/archive/screen-capture-scoping.md).
;; A user's captures are private to them: the gallery shows YOUR frames, never a
;; room's pool. Agents see a room's granted streams via screen_look, not here.
(defn-spin-remote search-screens!
  [server-id query]
  (spin-remote server-id [query]
    (let [q (identity query)]
      #?(:clj
         (let [party-id (access/authenticated-party-id)]
           (if party-id
             {:status :ok
              :query q
              :items ((requiring-resolve 'is.simm.runtimes.screen-intake/search-own)
                      party-id q 30)}
             {:status :error :error :authentication-required}))
         :cljs nil))))

;; Owner controls: delete a screenshot, list/delete recordings — all against the
;; caller's OWN captures (handler keys on the principal, never a client id).
(defn-spin-remote delete-screenshot!
  [server-id blob-id]
  (spin-remote server-id [blob-id]
    #?(:clj
       (let [party-id (access/authenticated-party-id)]
         (if party-id
           (merge {:status :ok}
                  ((requiring-resolve 'is.simm.runtimes.screen-intake/delete-frame!)
                   party-id blob-id))
           {:status :error :error :authentication-required}))
       :cljs nil)))

(defn-spin-remote list-recordings!
  [server-id]
  (spin-remote server-id []
    #?(:clj
       (let [party-id (access/authenticated-party-id)]
         (if party-id
           {:status :ok
            :sessions ((requiring-resolve 'is.simm.runtimes.screen-recording/list-sessions)
                       party-id)}
           {:status :error :error :authentication-required}))
       :cljs nil)))

(defn-spin-remote delete-recording!
  [server-id session-id-str]
  (spin-remote server-id [session-id-str]
    #?(:clj
       (let [party-id (access/authenticated-party-id)]
         (if party-id
           (merge {:status :ok}
                  ((requiring-resolve 'is.simm.runtimes.screen-recording/delete-session!)
                   party-id session-id-str))
           {:status :error :error :authentication-required}))
       :cljs nil)))

;; Web-page archive (doc/archive/web-intake-design.md) — the caller's OWN captured pages
;; (personal; handler keys on the principal).
(defn-spin-remote search-pages!
  [server-id query]
  (spin-remote server-id [query]
    #?(:clj
       (let [party-id (access/authenticated-party-id)]
         (if party-id
           {:status :ok :query query
            :items ((requiring-resolve 'is.simm.runtimes.web-intake/search)
                    party-id query 50)}
           {:status :error :error :authentication-required}))
       :cljs nil)))

(defn-spin-remote delete-page!
  [server-id page-id-str]
  (spin-remote server-id [page-id-str]
    #?(:clj
       (let [party-id (access/authenticated-party-id)]
         (if party-id
           (merge {:status :ok}
                  ((requiring-resolve 'is.simm.runtimes.web-intake/delete-page!)
                   party-id page-id-str))
           {:status :error :error :authentication-required}))
       :cljs nil)))

;; =============================================================================
;; Agents (create/update/delete + add-to/remove-from room)
;; =============================================================================

;; Create an agent party owned by owner-id and add it to the room.
(defn-spin-remote add-agent-to-room!
  [server-id room-id-str owner-id-str agent-name template-id-str]
  (spin-remote server-id [room-id-str owner-id-str agent-name template-id-str]
    (let [rid  (identity room-id-str)
          oid  (identity owner-id-str)
          aname (identity agent-name)
          tid  (identity template-id-str)]
      #?(:clj
         (let [room-uuid  (java.util.UUID/fromString rid)
               owner-uuid (java.util.UUID/fromString oid)
               tmpl       (when (seq tid) (templates/get-template tid))
               display    (if (seq aname) aname (or (:name tmpl) "Agent"))
               agent (parties/create-agent! owner-uuid
                       (cond-> {:display-name display
                                :auto-respond? true}
                         tmpl (merge {:template (:id tmpl)
                                      :model (:model tmpl)
                                      :provider (:provider tmpl)
                                      :system-prompt (:system-prompt tmpl)})))]
           (rooms/add-party! room-uuid (:party/id agent))
           (when-let [slug (:room/slug (rooms/get-room room-uuid))]
             (room-agents/assign-room-agent!
              slug (room-agents/party->actor-kw agent)
              {:role :specialist :response-policy :always}))
           {:status :ok :agent-id (str (:party/id agent))})
         :cljs nil))))

(defn-spin-remote update-agent-config!
  [server-id agent-id-str agent-name model system-prompt]
  (spin-remote server-id [agent-id-str agent-name model system-prompt]
    (let [aid (identity agent-id-str)
          an  (identity agent-name)
          m   (identity model)
          sp  (identity system-prompt)]
      #?(:clj
         (let [agent-uuid (java.util.UUID/fromString aid)]
           (parties/update-agent! agent-uuid
             (cond-> {}
               (seq an) (assoc :party/display-name an)
               (seq m)  (assoc :party/model m)
               (some? sp) (assoc :party/system-prompt sp)))
           {:status :ok})
         :cljs nil))))

(defn-spin-remote update-agent-assignment!
  [server-id room-id-str agent-id-str role-kw response-policy-kw]
  (spin-remote server-id [room-id-str agent-id-str role-kw response-policy-kw]
    (let [rid (identity room-id-str)
          aid (identity agent-id-str)
          role (identity role-kw)
          response-policy (identity response-policy-kw)]
      #?(:clj
         (let [room-uuid (java.util.UUID/fromString rid)
               agent-uuid (java.util.UUID/fromString aid)
               room (rooms/get-room room-uuid)]
           (if-let [slug (:room/slug room)]
             (if-let [assignment (room-agents/assign-room-agent!
                                   slug (room-agents/party->actor-kw agent-uuid)
                                   {:role role :response-policy response-policy})]
               (do
                 (urb/notify-parties! (:room/parties room))
                 {:status :ok
                  :assignment (-> assignment
                                  (update :assignment/id str)
                                  (update :assignment/room-id str)
                                  (update :assignment/created-at #(when % (str %)))
                                  (update :assignment/updated-at #(when % (str %))))})
               {:status :error :error :assignment-api-unavailable})
             {:status :error :error :room-has-no-dvergr-slug}))
         :cljs nil))))

;; Removes an agent party from a room. If the agent has no other rooms,
;; also deletes the party entirely.
(defn-spin-remote remove-agent-from-room!
  [server-id room-id-str agent-id-str]
  (spin-remote server-id [room-id-str agent-id-str]
    (let [rid (identity room-id-str)
          aid (identity agent-id-str)]
      #?(:clj
         (let [room-uuid  (java.util.UUID/fromString rid)
               agent-uuid (java.util.UUID/fromString aid)
               room (rooms/get-room room-uuid)]
           (when-let [slug (:room/slug room)]
             (room-agents/unassign-room-agent!
              slug (room-agents/party->actor-kw agent-uuid)))
           (rooms/remove-party! room-uuid agent-uuid)
           (when (empty? (rooms/get-party-rooms agent-uuid))
             (parties/delete-party! agent-uuid))
           {:status :ok})
         :cljs nil))))

;; =============================================================================
;; Room membership (humans)
;; =============================================================================

(defn-spin-remote add-room-party!
  [server-id room-id-str party-id-str]
  (spin-remote server-id [room-id-str party-id-str]
    (let [rid (identity room-id-str)
          pid (identity party-id-str)]
      #?(:clj
         (let [room-uuid (java.util.UUID/fromString rid)
               party-uuid (java.util.UUID/fromString pid)]
           (rooms/add-party! room-uuid party-uuid)
           {:status :ok})
         :cljs nil))))

(defn-spin-remote remove-room-party!
  [server-id room-id-str party-id-str]
  (spin-remote server-id [room-id-str party-id-str]
    (let [rid (identity room-id-str)
          pid (identity party-id-str)]
      #?(:clj
         (let [room-uuid (java.util.UUID/fromString rid)
               party-uuid (java.util.UUID/fromString pid)]
           (rooms/remove-party! room-uuid party-uuid)
           {:status :ok})
         :cljs nil))))

;; =============================================================================
;; Budget + context stats
;; =============================================================================

(defn-spin-remote update-room-budget!
  [server-id room-id-str budget-dollars]
  (spin-remote server-id [room-id-str budget-dollars]
    #?(:clj
       (let [room-uuid (java.util.UUID/fromString (identity room-id-str))
             dollars   (identity budget-dollars)]
         (rooms/set-room-budget! room-uuid dollars)
         ;; Also raise the LIVE agent ctx budgets: an agent paused at a
         ;; budget checkpoint (the "⚠️ budget exhausted" room warning)
         ;; resolves :extended and continues instead of wrapping up.
         (let [raised (try
                        (when-let [slug (:room/slug (rooms/get-room room-uuid))]
                          ((requiring-resolve 'dvergr.agent.room-context/raise-budget!)
                           ((requiring-resolve 'dvergr.room.store/slug->room-id) slug)
                           dollars))
                        (catch Throwable _ nil))]
           {:status :ok :live-ctxs-raised (or raised 0)}))
       :cljs nil)))

;; =============================================================================
;; Message dispatch (sender + agent responses)
;; =============================================================================

(defn-spin-remote dispatch-message!
  [server-id room-id-str sender-id-str content msg-uuid-str]
  (spin-remote server-id [room-id-str sender-id-str content msg-uuid-str]
    (let [rid (identity room-id-str)
          sid (identity sender-id-str)
          msg (identity content)
          muid (identity msg-uuid-str)]
      #?(:clj
         (let [;; Authoritative identity: the connection's JWT principal.
               ;; The client-passed sid is only compared for the transitional
               ;; mismatch warning (see is.simm.model.access).
               sender-id  (access/authenticated-party-id)
               _ (access/warn-on-sender-mismatch! sid sender-id ::dispatch-message!)
               room-uuid  (java.util.UUID/fromString rid)
               room-info  (rooms/get-room room-uuid)
               db-scope   (:room/content-db-scope room-info)
               room-conn  (when db-scope (room-dbs/connect-room-database db-scope))]
           (when room-conn
             (ensure-room-chatroom! room-conn room-uuid (or (:room/name room-info) "Chat"))
             (when sender-id
               (ensure-party-projection! room-conn sender-id))
             ;; Fire-and-forget: post into the room's dvergr.discourse Room
             ;; and return immediately. The room projector persists the send
             ;; (and the replies) from the bus; the client renders the send
             ;; optimistically under `muid` and reconciles on the sync echo.
             (room-agents/post-user-message! room-uuid msg sender-id room-conn
                                             (when muid (java.util.UUID/fromString muid)))))
         :cljs nil))))

;; =============================================================================
;; Knowledge bases
;; =============================================================================

(defn-spin-remote create-kb!
  [server-id party-id-str kb-name]
  (spin-remote server-id [party-id-str kb-name]
    (let [pid (identity party-id-str)
          kbn (identity kb-name)]
      #?(:clj (let [party-id (java.util.UUID/fromString pid)
                    kb (kbs/create-kb! party-id kbn)
                    server (when-let [get-server (resolve 'is.simm.runtimes.web/get-server)]
                             (get-server))]
                (when server
                  (kbs/register-kb-for-sync! (:kb/db-scope kb) server))
                {:kb/id (str (:kb/id kb))
                 :kb/name (:kb/name kb)
                 :kb/db-scope (str (:kb/db-scope kb))})
         :cljs nil))))

(defn-spin-remote attach-kb-to-room!
  [server-id room-id-str kb-id-str]
  (spin-remote server-id [room-id-str kb-id-str]
    (let [rid (identity room-id-str)
          kid (identity kb-id-str)]
      #?(:clj (let [room-uuid (java.util.UUID/fromString rid)
                    kb-uuid (java.util.UUID/fromString kid)]
                (kbs/attach-kb-to-room! room-uuid kb-uuid)
                (room-agents/reset-room-context! room-uuid)
                {:status :ok})
         :cljs nil))))

(defn-spin-remote detach-kb-from-room!
  [server-id room-id-str kb-id-str]
  (spin-remote server-id [room-id-str kb-id-str]
    (let [rid (identity room-id-str)
          kid (identity kb-id-str)]
      #?(:clj (let [room-uuid (java.util.UUID/fromString rid)
                    kb-uuid (java.util.UUID/fromString kid)]
                (kbs/detach-kb-from-room! room-uuid kb-uuid)
                (room-agents/reset-room-context! room-uuid)
                {:status :ok})
         :cljs nil))))

(defn-spin-remote delete-kb!
  [server-id kb-id-str]
  (spin-remote server-id [kb-id-str]
    (let [kid (identity kb-id-str)]
      #?(:clj (let [kb-uuid (java.util.UUID/fromString kid)]
                (kbs/delete-kb! kb-uuid)
                {:status :ok})
         :cljs nil))))

(defn-spin-remote share-kb!
  [server-id kb-id-str party-id-str]
  (spin-remote server-id [kb-id-str party-id-str]
    (let [kid (identity kb-id-str)
          pid (identity party-id-str)]
      #?(:clj (let [kb-uuid (java.util.UUID/fromString kid)
                    party-uuid (java.util.UUID/fromString pid)]
                (kbs/share-kb! kb-uuid party-uuid)
                {:status :ok})
         :cljs nil))))

(defn-spin-remote unshare-kb!
  [server-id kb-id-str party-id-str]
  (spin-remote server-id [kb-id-str party-id-str]
    (let [kid (identity kb-id-str)
          pid (identity party-id-str)]
      #?(:clj (let [kb-uuid (java.util.UUID/fromString kid)
                    party-uuid (java.util.UUID/fromString pid)]
                (kbs/unshare-kb! kb-uuid party-uuid)
                {:status :ok})
         :cljs nil))))

(defn-spin-remote load-kb-details!
  [server-id kb-id-str]
  (spin-remote server-id [kb-id-str]
    (let [kid (identity kb-id-str)]
      #?(:clj (let [kb-uuid (java.util.UUID/fromString kid)
                    kb (kbs/get-kb kb-uuid)
                    all-humans (parties/list-parties :human)
                    attached-rooms (kbs/rooms-using-kb kb-uuid)
                    shared-with (or (:kb/shared-with kb) [])
                    shared-parties (->> shared-with
                                        (mapv parties/get-party)
                                        (filterv some?))]
                {:kb (dissoc kb :kb/shared-with)
                 :attached-rooms attached-rooms
                 :shared-parties shared-parties
                 :all-humans all-humans})
         :cljs nil))))

;; =============================================================================
;; Contacts (party-to-party edges)
;; =============================================================================

(defn-spin-remote add-contact-by-handle!
  [server-id party-id-str handle]
  (spin-remote server-id [party-id-str handle]
    (let [pid (identity party-id-str)
          h   (identity handle)]
      #?(:clj (let [party-id (java.util.UUID/fromString pid)]
                (if-let [contact-id (parties/find-contact-by-handle h)]
                  (do (parties/add-contact! party-id contact-id)
                      {:status :ok :contact-id (str contact-id)})
                  {:status :not-found :handle h}))
         :cljs nil))))

(defn-spin-remote add-contact!
  [server-id party-id-str contact-id-str]
  (spin-remote server-id [party-id-str contact-id-str]
    (let [pid (identity party-id-str)
          cid (identity contact-id-str)]
      #?(:clj (let [party-id (java.util.UUID/fromString pid)
                    contact-id (java.util.UUID/fromString cid)]
                (parties/add-contact! party-id contact-id)
                {:status :ok})
         :cljs nil))))

(defn-spin-remote remove-contact!
  [server-id party-id-str contact-id-str]
  (spin-remote server-id [party-id-str contact-id-str]
    (let [pid (identity party-id-str)
          cid (identity contact-id-str)]
      #?(:clj (let [party-id (java.util.UUID/fromString pid)
                    contact-id (java.util.UUID/fromString cid)]
                (parties/remove-contact! party-id contact-id)
                {:status :ok})
         :cljs nil))))

;; =============================================================================
;; Drives (files panel) — doc/archive/file-system-design.md. The tree is
;; serialized server-side (fs.node maps); file bytes never travel this
;; path (upload = POST /blobs, download = GET /blobs/<hash>).
;; =============================================================================

(defn-spin-remote load-room-drive!
  [server-id room-id-str cut-ms]
  (spin-remote server-id [room-id-str cut-ms]
    (let [rid (identity room-id-str)
          cut (identity cut-ms)]
      #?(:clj
         (let [party-id (access/authenticated-party-id)
               room-uuid (java.util.UUID/fromString rid)
               room (rooms/get-room room-uuid)
               drive (drives/ensure-room-drive! room-uuid
                                                :owner-id party-id
                                                :room-name (:room/name room))]
           (when drive
             (when-let [conn (drives/connect-drive-database (:drive/db-scope drive))]
               ;; GlobalCut: server-side d/as-of on the drive store
               ;; (keep-history? true). File ops are transacted per-write,
               ;; so tx-time ≈ file-time — no sent-at divergence. drives/ls
               ;; derefs its arg, so an atom holding the as-of db is a
               ;; drop-in 'conn' without touching dvergr.
               (let [tconn (if cut
                             (atom (d/as-of @conn (java.util.Date. (long cut))))
                             conn)]
                 {:drive (select-keys drive [:drive/id :drive/name :drive/db-scope])
                  :tree (drives/tree tconn)
                  :cut cut}))))
         :cljs nil))))

(defn-spin-remote put-drive-file!
  [server-id room-id-str dir-path file-name blob-id mime size]
  (spin-remote server-id [room-id-str dir-path file-name blob-id mime size]
    (let [rid (identity room-id-str)]
      #?(:clj
         (let [room-uuid (java.util.UUID/fromString rid)]
           (when-let [drive (first (drives/get-room-drives room-uuid))]
             (when-let [conn (drives/connect-drive-database (:drive/db-scope drive))]
               (let [segs (vec (remove str/blank?
                                       (str/split (or dir-path "") #"/")))
                     dir-id (drives/ensure-path! conn segs)
                     node (drives/link-blob! conn dir-id file-name blob-id mime size :upload)]
                 {:status :ok :node-id (str (:fs.node/id node))}))))
         :cljs nil))))

(defn-spin-remote delete-drive-node!
  [server-id room-id-str node-id-str]
  (spin-remote server-id [room-id-str node-id-str]
    (let [rid (identity room-id-str)
          nid (identity node-id-str)]
      #?(:clj
         (let [room-uuid (java.util.UUID/fromString rid)]
           (when-let [drive (first (drives/get-room-drives room-uuid))]
             (when-let [conn (drives/connect-drive-database (:drive/db-scope drive))]
               (try (drives/rm! conn (java.util.UUID/fromString nid))
                    {:status :ok}
                    (catch Exception e
                      {:status :error :message (ex-message e)})))))
         :cljs nil))))

(defn-spin-remote mkdir-drive-path!
  [server-id room-id-str dir-path]
  (spin-remote server-id [room-id-str dir-path]
    (let [rid (identity room-id-str)]
      #?(:clj
         (let [room-uuid (java.util.UUID/fromString rid)]
           (when-let [drive (first (drives/get-room-drives room-uuid))]
             (when-let [conn (drives/connect-drive-database (:drive/db-scope drive))]
               (let [segs (vec (remove str/blank?
                                       (str/split (or dir-path "") #"/")))]
                 {:status :ok :dir-id (str (drives/ensure-path! conn segs))}))))
         :cljs nil))))

(defn-spin-remote create-drive!
  [server-id drive-name]
  (spin-remote server-id [drive-name]
    (let [dn (identity drive-name)]
      #?(:clj
         (let [party-id (access/authenticated-party-id)]
           (if party-id
             (let [drive (drives/create-drive! party-id dn)]
               {:status :ok :drive-id (str (:drive/id drive))})
             {:status :error :message "not authenticated"}))
         :cljs nil))))

(defn-spin-remote attach-drive-to-room!
  [server-id room-id-str drive-id-str]
  (spin-remote server-id [room-id-str drive-id-str]
    (let [rid (identity room-id-str)
          did (identity drive-id-str)]
      #?(:clj (let [room-uuid (java.util.UUID/fromString rid)
                    drive-uuid (java.util.UUID/fromString did)]
                (drives/attach-drive-to-room! room-uuid drive-uuid)
                (room-agents/reset-room-context! room-uuid)
                {:status :ok})
         :cljs nil))))

(defn-spin-remote detach-drive-from-room!
  [server-id room-id-str drive-id-str]
  (spin-remote server-id [room-id-str drive-id-str]
    (let [rid (identity room-id-str)
          did (identity drive-id-str)]
      #?(:clj (let [room-uuid (java.util.UUID/fromString rid)
                    drive-uuid (java.util.UUID/fromString did)]
                (drives/detach-drive-from-room! room-uuid drive-uuid)
                (room-agents/reset-room-context! room-uuid)
                {:status :ok})
         :cljs nil))))
