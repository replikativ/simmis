(ns is.simm.ops.github-candidate-roots
  "HOST-only preparation retention. One immutable provenance blueprint holds
   every root ID before native IO. The original strict transaction winner
   creates; recovery only verifies the same roots. No per-root lifecycle,
   source-selected callback, cursor advancement or Proposal acceptance."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [is.simm.model.github-proposals :as provenance]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.checkpoint-roots :as physical]
            [is.simm.ops.github-roots :as bases]
            [is.simm.ops.knowledge-changes :as changes]))

(defn- refuse! [reason]
  (throw (ex-info "Git candidate retention is held" {:type ::held :reason reason})))

(defn descriptor
  "Select an exact published native snapshot. This allocates an inert root ID,
   not a native pin or permission to construct one."
  [db head]
  (when-not (uuid? head) (refuse! :invalid-head))
  {:id (random-uuid) :head head :basis (changes/basis db)})

(defn- owner [body role]
  {:simmis/github-preparation (get-in body [:producer :sync-id])
   :claim (get-in body [:producer :claim]) :by (get-in body [:producer :by])
   :room (get-in body [:identity :connection :room])
   :root (get-in body [:roots role :id]) :role role
   :provenance-id (provenance/provenance-id body)})

(defn- original-snapshot! [source basis]
  (let [snapshot (d/commit-as-db (:store source) (:commit basis))]
    (when-not (and snapshot (= (dissoc (:config source) :branch)
                               (dissoc (:config snapshot) :branch)))
      (refuse! :physical-source-changed))
    ;; Use the actual snapshot's branch/configuration. A parent's live branch
    ;; is not the branch under which the private checkpoint was published.
    (physical/load-snapshot! snapshot basis)))

(defn- visible-head [db repository]
  (let [ref (:geschichte.repo/head (d/pull db [:geschichte.repo/head] [:geschichte.repo/id repository]))]
    (get-in (d/pull db '[{:geschichte.ref/target [:geschichte.commit/id]}] [:geschichte.ref/name ref])
            [:geschichte.ref/target :geschichte.commit/id])))

(defn- snapshot-correspondence! [source body]
  ;; Physical retention is not a semantic merge certificate. Still bind each
  ;; role to the actual logical/native snapshot named by the fixed planner.
  (let [repository (get-in body [:identity :connection :target :repository])
        local (original-snapshot! source (get-in body [:roots :local :basis]))
        candidate (original-snapshot! source (get-in body [:roots :candidate :basis]))
        incoming (get-in body [:identity :incoming])
        commit (d/pull candidate [:geschichte.commit/id :geschichte.commit/snapshot :geschichte.commit/git-oid]
                       [:geschichte.commit/id (:head incoming)])]
    (when-not (and (= (get-in body [:proposal :base]) (visible-head local repository))
                   (= (get-in body [:proposal :head]) (visible-head candidate repository))
                   (= (:head incoming) (:geschichte.commit/id commit))
                   (= (:git-oid incoming) (:geschichte.commit/git-oid commit))
                   (= (get-in body [:roots :incoming :basis :commit]) (:geschichte.commit/snapshot commit)))
      (refuse! :snapshot-correspondence-changed))))

(defn- correspondence! [row body]
  (provenance/validate-provenance! body)
  (when-not (and (= :incoming (journal/kind row))
                 (= (get-in body [:producer :sync-id]) (:github.sync/id row))
                 (= (get-in body [:producer :claim]) (:github.sync/claim row))
                 (= (get-in body [:producer :by]) (:github.sync/by row))
                 (= (get-in body [:identity :connection :room]) (:github.sync/room row)))
    (refuse! :different-intent)))

(defn- retained! [control expected body]
  (let [row (journal/sync-in @control (:github.sync/id expected))]
    (when-not (= expected row) (refuse! :intent-changed))
    (correspondence! row body)
    (when-not (and (#{:retaining :ready :filing :filed :closing :closed :held} (:github.sync/state row))
                   (= (provenance/provenance-id body) (:github.sync/retention-id row))
                   (= body (:value (artifact/get-bounded-value (artifact/datahike-store control)
                                                               (:github.sync/retention row) 32768))))
      (refuse! :retention-unavailable))
    row))

(defn verify!
  "Verify ALL original roots from the exact retained blueprint. A missing or
   partial outcome holds; it never permits creation. Current eligibility and
   source authority are supplied by the fixed HOST check!, not these values."
  [control source expected body check!]
  (when-not (fn? check!) (refuse! :missing-host-boundary))
  (check!)
  (retained! control expected body)
  (snapshot-correspondence! source body)
  (let [connection (get-in body [:identity :connection])
        base (bases/verify-base! control source connection (get-in body [:roots :base :id]) check!)
        cursor (:cursor base)]
    (when-not (and (= (:selection base)
                      {:head (get-in body [:roots :base :head])
                       :git-oid (get-in body [:identity :cursor :git-oid])
                       :basis (get-in body [:roots :base :basis])})
                   (= (get-in body [:identity :cursor])
                      {:head (:github.integration/head cursor) :git-oid (:github.integration/oid cursor)
                       :proof-id (:github.integration/proof-id cursor)}))
      (refuse! :base-proof-changed)))
  (doseq [role [:local :incoming :candidate]]
    (check!)
    (let [{:keys [id basis]} (get-in body [:roots role])
          snapshot (original-snapshot! source basis)]
      (physical/verify! snapshot basis id (owner body role))))
  (check!)
  (retained! control expected body)
  {:provenance body :row expected})

(defn retain!
  "Only the original allocated private owner calls this once. Persist the exact
   blueprint and receive a POSITIVE transaction ACK before creating any root.
   A lost ACK exits before creation. A later caller uses verify! exclusively.
   check!/writer-check! are mandatory fixed HOST lifetime/authority callbacks."
  [control source expected body check! writer-check!]
  (when-not (and (fn? check!) (fn? writer-check!)) (refuse! :missing-host-boundary))
  (check!)
  (correspondence! expected body)
  (snapshot-correspondence! source body)
  (when-not (= :allocated (:github.sync/state expected)) (refuse! :not-creation-owner))
  (let [digest (provenance/provenance-id body) report (volatile! nil)]
    (artifact/publish-value!
     (artifact/datahike-store control) body
     (fn [ref]
       (check!)
       (vreset! report
                (d/transact control
                            [[:db.fn/call (fn [db]
                                            (writer-check! db)
                                            (journal/transition-tx db expected :retaining ref digest))]]))))
    (when-not (and @report (= expected (journal/sync-in (:db-before @report) (:github.sync/id expected))))
      (refuse! :creation-unconfirmed))
    (let [row (journal/sync-in @control (:github.sync/id expected))]
      (retained! control row body)
      (when-not (= :retaining (:github.sync/state row)) (refuse! :creation-unconfirmed))
      (doseq [role [:local :incoming :candidate]]
        (check!)
        (retained! control row body)
        (let [{:keys [id basis]} (get-in body [:roots role])
              snapshot (original-snapshot! source basis)]
          (physical/create! snapshot basis id (owner body role))))
      (verify! control source row body check!))))
