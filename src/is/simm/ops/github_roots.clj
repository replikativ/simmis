(ns is.simm.ops.github-roots
  "Fixed HOST retention of the proven upstream base on its original store.
   Caller holds original Room/App lifetime and exclusive native operation
   admission. Trusted callbacks check current authority, including in the
   control writer. Never lifted into SCI or exposed as a pin API."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-pins :as pins]
            [is.simm.ops.checkpoint-roots :as physical]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.runtimes.storage-gc :as storage-gc]))

(defn- refuse! [reason]
  (throw (ex-info "Git base retention is held" {:type ::held :reason reason})))

(defn- selection! [source connection cursor]
  (when-not (= (select-keys (:target connection) [:repository :store :backend :branch :ref])
               (select-keys (changes/source-identity source) [:repository :store :backend :branch :ref]))
    (refuse! :physical-source-changed))
  (let [head (:github.integration/head cursor)
        commit (d/pull source [:geschichte.commit/id :geschichte.commit/snapshot :geschichte.commit/git-oid]
                       [:geschichte.commit/id head])
        checkpoint (:geschichte.commit/snapshot commit)
        snapshot (when (uuid? checkpoint) (d/commit-as-db source checkpoint))]
    (when-not (and (= head (:geschichte.commit/id commit))
                   (= (:github.integration/oid cursor) (:geschichte.commit/git-oid commit))
                   snapshot (= checkpoint (d/commit-id snapshot))
                   ;; Git logical snapshots may originate on a private branch;
                   ;; retain their ACTUAL configuration, never relabel it as
                   ;; the live parent. All other physical configuration matches.
                   (= (dissoc (:config source) :branch) (dissoc (:config snapshot) :branch)))
      (refuse! :base-snapshot-unavailable))
    (let [selection {:head head :git-oid (:geschichte.commit/git-oid commit)
                     :basis (changes/basis snapshot)}]
      (when-not (= (get-in connection [:target :repository]) (get-in selection [:basis :repository]))
        (refuse! :physical-source-changed))
      {:selection selection :snapshot snapshot})))

(defn- retained-body! [control row]
  (let [body (:value (artifact/get-bounded-value (artifact/datahike-store control)
                                                 (:github.pin/body row) 16384))]
    (when-not (and (= (pins/body-id body) (:github.pin/body-id row))
                   (= (pins/intent-key body) (:github.pin/key row))
                   (= (:root body) (:github.pin/id row))
                   (= (:claim body) (:github.pin/claim row))
                   (= (get-in body [:connection :room]) (:github.pin/room row))
                   (#{:creating :retained} (:github.pin/state row)))
      (refuse! :intent-changed))
    body))

(defn- owner [body]
  {:simmis/github-base (:root body) :claim (:claim body) :by (:by body)
   :room (get-in body [:connection :room])
   :connection-id (connections/connection-id (:connection body))
   :body-id (pins/body-id body)})

(defn retain-base!
  "Retain a verified cursor's published base before preparing Git changes.
   A positively acknowledged fresh intent creates once. Every later attempt
   verifies that SAME native root, including after either lost ACK; absent or
   partial outcomes hold. No cleanup or inference from current HEAD occurs.

   Requires the HOST boot prohibition throughout snapshot construction/pinning,
   exclusive original native ownership and no independent store collectors.
   check! and writer-check! are mandatory fixed HOST callbacks, not request data."
  [control source connection by check! writer-check!]
  (when-not (and (fn? check!) (fn? writer-check!) (uuid? by))
    (refuse! :missing-host-boundary))
  (check!)
  ;; Also cover trusted non-web embedding entrypoints. If anything was already
  ;; admitted this installation refuses; it never pretends to drain that work.
  (storage-gc/install-git-retention-profile!)
  (connections/connection! connection)
  (let [cursor (or (integrations/current-in @control (:room connection))
                   (refuse! :integration-unverified))
        stored-connection (:value (artifact/get-bounded-value (artifact/datahike-store control)
                                                              (:github.integration/connection cursor) 16384))
        _ (when-not (and (= connection stored-connection)
                         (= (connections/connection-id connection) (:github.integration/connection-id cursor)))
            (refuse! :connection-changed))
        {:keys [selection snapshot]} (selection! source connection cursor)
        body {:version 1 :purpose :upstream-base :connection connection :cursor cursor
              :selection selection :root (random-uuid) :claim (random-uuid) :by by}
        key (pins/intent-key body)
        ack (volatile! nil)]
    (artifact/publish-value!
     (artifact/datahike-store control) body
     (fn [ref]
       (check!)
       ;; Capture the live HOST behind an opaque trusted callback. Never pass
       ;; the recursive runtime graph as transaction-function argument data.
       (vreset! ack (d/transact control
                                [[:db.fn/call (fn [db] (writer-check! db) (pins/prepare-tx db body ref))]]))))
    (let [row (or (pins/by-key @control key) (refuse! :intent-unconfirmed))
          original (retained-body! control row)
          fresh? (and @ack (nil? (pins/by-key (:db-before @ack) key))
                      (= (:root body) (:github.pin/id row))
                      (= (:claim body) (:github.pin/claim row)))]
      (when-not (= (select-keys original [:connection :cursor :selection])
                   (select-keys body [:connection :cursor :selection]))
        (refuse! :intent-changed))
      (check!)
      (pins/current-cursor! @control original)
      (when fresh?
        (physical/create! snapshot (:basis selection) (:root original) (owner original)))
      ;; Lost native ACK is recovered ONLY here on a later attempt. A missing
      ;; root after a positive control ACK is deliberately not recreated.
      (physical/verify! snapshot (:basis selection) (:root original) (owner original))
      (check!)
      (d/transact control [[:db.fn/call (fn [db]
                                          (writer-check! db)
                                          (pins/confirm-tx db row original))]])
      {:root (:root original) :selection selection :db snapshot})))
