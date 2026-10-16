(ns is.simm.agents.summarizer
  "Link-driven chat→wiki summarizer — the first one; see `merger` for the second.

   'Public compaction': a cheap one-shot LLM call (utility tier — NOT an
   agent turn) turns a window of room conversation into a concise,
   wikilink-forced summary, published as a RECORD page in the room's
   product KB (kind :chat-summary, room + window props). The [[links]]
   the model emits become :block/references datoms via the normal
   kb-upsert path, so every topic page's backlinks accumulate a
   chronology of the conversations that touched it — the wiki as the
   navigation layer over chats. A S/KBEvent marker row in the room
   content DB punctuates the chat timeline with a pointer to the page.

   Tiering (doc/kb-unification.md + 2026-07-05 discussion):
   - transform  = host utility call (this ns, cheap-llm-call)
   - trigger    = manual / room schedule (wiki/summarize! in the sandbox)
   - v2         = agent-judged merging of summaries into topic pages
   - v3         = unify with dvergr's context compaction (same summary,
                  second sink)."
  (:require [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.task-models :as task-models]
            [datahike.api :as d]
            [clojure.string :as str]
            [taoensso.telemere :as log]))

(def ^:private summary-prompt
  (str "Summarize this team chat conversation CONCISELY (at most 8 sentences). "
       "Wrap EVERY entity, project, person, decision and important topic in "
       "wikilinks like [[Topic Name]] — the links are how the knowledge base "
       "connects, so be generous and precise with them. Focus on decisions, "
       "outcomes, and open questions; skip pleasantries. Write plain prose, "
       "no headings or bullet lists."))

(defn- last-window-end
  "window-end of the room's most recent chat-summary record page, or nil."
  [kb-conn room-uuid]
  (some->> (d/q '[:find [?we ...] :in $ ?room :where
                  [?p :S.Page/kind :chat-summary]
                  [?p :S.Page/room ?room]
                  [?p :S.Page/window-end ?we]]
                @kb-conn room-uuid)
           seq (sort compare) last))

(defn- window-messages
  "Conversational messages (role :user/:assistant) from the room store
   after `since` (nil = from the beginning), oldest first, capped to the
   most recent `max-messages`."
  [room since max-messages]
  (->> (d/q '[:find ?ts ?role ?c ?su :where
              [?m :message/created-at ?ts]
              [?m :message/role ?role]
              [?m :message/content ?c]
              [(get-else $ ?m :message/source-user "") ?su]]
            @(:conn (:store room)))
       (filter (fn [[ts role]]
                 (and (contains? #{:user :assistant} role)
                      (or (nil? since) (pos? (compare ts since))))))
       (sort-by first)
       (take-last max-messages)))

(defn- transcript [msgs]
  (->> msgs
       (map (fn [[_ role c su]]
              (str (if (= role :assistant) "assistant" (if (seq su) su "user"))
                   ": " c)))
       (str/join "\n")))

(defn- marker-author-eid
  "An author entity for the KBEvent marker (timeline query joins through
   it): the first agent party projected into the content DB, else any
   S.User row."
  [room-conn room-uuid]
  (or (d/q '[:find ?e . :where [?e :S.User/is-ai true]] @room-conn)
      (d/q '[:find ?e . :where [?e :S.User/display-name _]] @room-conn)))

(defn summarize-room!
  "Summarize the room's conversation window since the last chat-summary
   record (whole recent history on first run) into a wikilinked record
   page + timeline marker. Returns {:page … :links n :messages n} or
   {:skipped reason}.

   `:kb-id` names the KB to write — the caller's choice, already authorized
   (`wiki/summarize!` passes a KB its sandbox may write). Without one there is
   no target: this used to take the room's first attached KB whatever its
   grant, a `:read` one included."
  [room-uuid live-room-fn & {:keys [max-messages min-messages model kb-id]
                             :or   {max-messages 150 min-messages 8}}]
  (let [room (live-room-fn room-uuid)
        kb   (some-> kb-id kbs/get-kb)
        kb-conn (some-> kb :kb/db-scope kbs/connect-kb-database)]
    (cond
      (nil? room)    {:skipped :no-live-room}
      (nil? kb-conn) {:skipped :no-kb}
      :else
      (let [since (last-window-end kb-conn room-uuid)
            msgs  (window-messages room since max-messages)]
        (if (< (count msgs) min-messages)
          {:skipped :window-too-small :messages (count msgs)}
          (let [text ((requiring-resolve 'dvergr.tools.llm-call/cheap-llm-call)
                      summary-prompt (transcript msgs)
                      ;; an explicit `model` wins; otherwise the `:cheap` task
                      ;; model, which throws (with its reason) when configured
                      ;; and unresolvable rather than using dvergr's default
                      (merge {:max-tokens 600}
                             (if model {:model model} (task-models/model-opts :cheap))))
                _ (when (:error text)
                    (throw (ex-info "summarizer llm call failed" text)))
                summary (:text text)
                w-start (ffirst msgs)
                w-end   (first (last msgs))
                room-name (or (:room/name (rooms/get-room room-uuid)) "Chat")
                title (str room-name " — "
                           (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm")
                                    w-end))
                page-uuid (kbs/kb-upsert-knowledge-page! kb-conn title
                                                         :summary summary)
                ;; record-page properties: kind + the value-level chat
                ;; backref (room uuid + window instants — no eids, no xref
                ;; machinery needed; doc/kb-unification.md)
                _ (d/transact kb-conn [{:entity/uuid page-uuid
                                        :S.Page/kind :chat-summary
                                        :S.Page/room room-uuid
                                        :S.Page/window-start w-start
                                        :S.Page/window-end w-end}])
                links (kbs/extract-wikilinks summary)
                ;; timeline marker in the content DB
                scope (room-dbs/get-room-db-scope room-uuid)
                room-conn (some-> scope room-dbs/connect-room-database)]
            (when room-conn
              (when-let [author (marker-author-eid room-conn room-uuid)]
                (d/transact room-conn
                  [{:entity/uuid (random-uuid)
                    :entity/created-at (java.util.Date.)
                    :instance/of-role [:entity/name "S/KBEvent"]
                    ;; ref attr — the content-DB room entity carries the
                    ;; room uuid as :entity/uuid
                    :S.KBEvent/room [:entity/uuid room-uuid]
                    :S.KBEvent/type :chat-summary
                    :S.KBEvent/title title
                    :S.KBEvent/entity-uuid page-uuid
                    :S.KBEvent/author author
                    :S.KBEvent/timestamp (java.util.Date.)}])))
            ;; v2: fold the summary's claims into each linked TOPIC page
            ;; (op-based, grounding-reviewed — is.simm.agents.merger).
            (let [merge-report
                  (try ((requiring-resolve 'is.simm.agents.merger/merge-summary!)
                        kb-conn page-uuid summary links)
                       (catch Throwable t
                         (log/log! {:level :warn :id ::merge-failed
                                    :data {:error (ex-message t)}})
                         nil))]
              (log/log! {:level :info :id ::summarized
                         :data {:room room-uuid :page title :links (count links)
                                :messages (count msgs) :merge merge-report}})
              {:page title :page-uuid page-uuid
               :links links :messages (count msgs)
               :merge merge-report})))))))
