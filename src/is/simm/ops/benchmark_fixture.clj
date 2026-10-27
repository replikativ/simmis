(ns is.simm.ops.benchmark-fixture
  "Fixed fresh benchmark Rooms and exact native ownership receipts. Native
   preparation/registration recovery stays in Dvergr's existing owners. A name
   collision or missing receipt never grants permission to close a Room."
  (:require [clojure.java.io :as io]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.room.registry :as registry]
            [dvergr.rooms :as rooms]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.system.db :as system-db]
            [dvergr.system.rooms :as native]
            [geschichte.git.http :as git-http]
            [geschichte.git.local :as git-local]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [org.replikativ.spindel.engine.core :as ec]))

(def profile :simmis/benchmark-fixture-v1)

(defn slug [request-id]
  (when-not (uuid? request-id) (throw (ex-info "Benchmark Request UUID required" {})))
  (str "simmis-benchmark-" request-id))

(defn- refuse! [reason]
  (throw (ex-info "Benchmark fixture requires reconciliation" {:type ::held :reason reason})))

(defn- profile! [options]
  (when-not (= profile (:profile options)) (refuse! :profile-mismatch)))

(defn- prepare-open! [conn options]
  (profile! options)
  (let [db @conn]
    (when-not (and (uuid? (:geschichte.repo/id (query/repository db)))
                   (= 1 (count (take 2 (d/datoms db :aevt :geschichte.repo/id)))))
      (refuse! :invalid-repository))))

(defn policies
  "Trusted leaf policy shared by fresh creation and boot hydration. Import the
   host's configured sandbox template once; never import on open or fall back
   after a failed import. No Request, App owner or ticket is captured here."
  []
  {profile {:initialize! (fn [conn options]
                           (profile! options)
                           (repo/init! conn {:name "Benchmark fixture"})
                           (let [source (workspace/sandbox-repo)]
                             (if-let [local (git-local/source-file (io/file ".") source)]
                               (git-local/import! conn local {:remote "upstream" :clone? true})
                               (git-http/clone! conn "upstream" source)))
                           (prepare-open! conn options))
            :prepare-open! prepare-open!
            ;; The initializer installs no separate governance or async worker.
            ;; The existing initialization owner releases the connection.
            :abort-create! (fn [_ _] nil)}})

(defn validate-receipt!
  "Inspect exact already-created ownership. No hydration, provisioning, source
   selection or caller-supplied fallback. This is a HOST-only native receipt."
  [host-ctx request-id {:keys [room persistent-id] :as receipt}]
  (when-not (and (map? receipt) (= #{:room :persistent-id} (set (keys receipt)))
                 (uuid? persistent-id) room)
    (refuse! :fixture-owner-changed))
  (binding [ec/*execution-context* host-ctx]
    (let [expected-slug (slug request-id)
          db @(system-db/get-conn)
          row (d/entity db [:room/id persistent-id])
          live (registry/lookup (keyword expected-slug))]
      (when-not (and (= expected-slug (:room/slug row) (:slug room))
                     (= (keyword expected-slug) (:id room))
                     (= profile (:system/repository-profile (:room/default-repository row)))
                     (identical? room live) (identical? (:ctx room) (native/room-ctx-for persistent-id))
                     (binding [ec/*execution-context* (:ctx room)] (registry/admitted-incarnation? room)))
        (refuse! :fixture-owner-changed))
      receipt)))

(defn create!
  "Fresh-only native creation. Failed provisioning/registration keeps its
   upstream recovery owner; a receipt is returned only after full registration.
   Caller records that receipt before attempting Request publication."
  [host-ctx request-id]
  (binding [ec/*execution-context* host-ctx]
    (let [provisioned (native/provision-profiled-room!
                       {:slug (slug request-id) :name "Benchmark fixture" :type :project
                        :repository-profile profile} (policies))
          id (:room-id provisioned)
          room (rooms/register-prepared-room! id)]
      (validate-receipt! host-ctx request-id {:room room :persistent-id id}))))

(defn retire!
  "Close only this captured native incarnation and archive its exact persistent
   UUID. Human revocation does not revoke cleanup ownership. A replacement
   incarnation is held for reconciliation; it is never closed by name."
  [host-ctx request-id {:keys [room persistent-id] :as receipt}]
  (when-not (and (map? receipt) (= #{:room :persistent-id} (set (keys receipt)))
                 (uuid? persistent-id) room)
    (refuse! :fixture-owner-changed))
  (binding [ec/*execution-context* host-ctx]
    (let [expected-slug (slug request-id)
          db @(system-db/get-conn) row (d/entity db [:room/id persistent-id])
          live (registry/lookup (keyword expected-slug))]
      (when-not (and (uuid? persistent-id) room
                     (= expected-slug (:room/slug row) (:slug room))
                     (= (keyword expected-slug) (:id room))
                     (= profile (:system/repository-profile (:room/default-repository row)))
                     (or (nil? live) (identical? room live)))
        (refuse! :fixture-owner-changed))
      (discourse/close-room! room)
      ;; Closing releases its lifecycle reservation. Reserve the now-empty
      ;; slot again before the archive write, so replacement hydration cannot
      ;; interleave between a check and persistent retirement. A replacement
      ;; that already won is held, never adopted by this old receipt.
      (registry/with-close-reservation!
        room :removed
        (fn [_]
          (system-db/archive-room! persistent-id)
          (let [archived (d/entity @(system-db/get-conn) [:room/id persistent-id])]
            (when-not (and (= expected-slug (:room/slug archived))
                           (= profile (:system/repository-profile (:room/default-repository archived)))
                           (inst? (:room/archived-at archived)))
              (refuse! :fixture-archive-unconfirmed)))))
      ;; The owner layer takes its own removed-slot extent. Closing/archiving
      ;; alone does not release the published native acquisition journal.
      (native/retire-room-runtime! persistent-id room)
      true)))
