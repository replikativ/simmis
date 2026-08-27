(ns is.simm.agents.dispatch
  "Pure Simmis policy projection over dvergr room assignments.

   Dvergr owns durable actors, assignments and message delivery. This namespace
   resolves presentation-level @handles against Simmis parties and decides which
   assigned agent actors should be awakened for one inbound message."
  (:require [clojure.string :as str]
            [is.simm.model.references :as refs]))

(defn party->actor-id
  "The canonical dvergr actor id for a Simmis party."
  [party-or-id]
  (let [party-id (if (map? party-or-id) (:party/id party-or-id) party-or-id)]
    (keyword "party" (str party-id))))

(defn- normalized-handle [handle]
  (some-> handle str str/lower-case))

(defn- invalid-mentions! [unknown ambiguous]
  (when (or (seq unknown) (seq ambiguous))
    (throw (ex-info "Some message mentions do not resolve uniquely in this room"
                    {:type :dispatch/invalid-mentions
                     :unknown (set unknown)
                     :ambiguous (into {} ambiguous)}))))

(defn plan-message-dispatch
  "Resolve message mentions and select automatic agent recipients.

   `room-parties` are Simmis party maps. `assignments` are normalized dvergr
   assignment maps. Response policies mean:

   - `:always`  — wake for every inbound message;
   - `:mention` — wake only when the actor's room-local handle is mentioned;
   - `:manual`  — never wake from an ordinary posted message.

   Old rooms without assignments retain their previous behavior: an agent with
   `:party/auto-respond?` defaults to `:always`, otherwise `:manual`.

   Explicit unknown or case-insensitively ambiguous handles throw instead of
   falling back to every agent. Human mentions are valid audience but never
   become automatic agent recipients."
  [room-parties assignments content]
  (let [mentions (->> (refs/extract-user-mentions content)
                      (map normalized-handle)
                      set)
        by-handle (->> room-parties
                       (keep (fn [party]
                               (when-let [handle (normalized-handle (:party/handle party))]
                                 [handle party])))
                       (group-by first)
                       (map (fn [[handle entries]]
                              [handle (mapv second entries)]))
                       (into {}))
        unknown (filter #(empty? (get by-handle %)) mentions)
        ambiguous (keep (fn [handle]
                          (let [matches (get by-handle handle)]
                            (when (> (count matches) 1)
                              [handle (mapv party->actor-id matches)])))
                        mentions)
        _ (invalid-mentions! unknown ambiguous)
        mentioned-parties (mapcat #(get by-handle %) mentions)
        mentioned-actors (into #{} (map party->actor-id) mentioned-parties)
        assignment-by-actor (into {} (map (juxt :assignment/actor-id identity))
                                  assignments)
        policy-for (fn [party]
                     (or (get-in assignment-by-actor
                                 [(party->actor-id party)
                                  :assignment/response-policy])
                         (if (:party/auto-respond? party) :always :manual)))
        recipients (->> room-parties
                        (filter #(= :agent (:party/type %)))
                        (filter (fn [party]
                                  (case (policy-for party)
                                    :always true
                                    :mention (contains? mentioned-actors
                                                        (party->actor-id party))
                                    :manual false
                                    false)))
                        vec)]
    {:mentions mentions
     :audience (->> mentioned-actors (sort-by str) vec)
     :recipients recipients}))
