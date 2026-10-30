(ns is.simm.model.github-proposals
  "Git provenance markers on ordinary Proposals. A marker requires the fixed
   Git review/decision boundary even when the retained provenance is malformed.
   It is never permission to use the generic branch settlement backend."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.knowledge-streams :as streams]
            [malli.core :as m]))

(def markers [:proposal/github-key :proposal/github-provenance :proposal/github-provenance-id])

(def schema
  [{:db/ident :proposal/github-key :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :proposal/github-provenance :db/valueType :db.type/store-ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :proposal/github-provenance-id :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one}])

(defn marked? [proposal]
  (boolean (some #(contains? proposal %) markers)))

(defn marked-in? [db id]
  ;; Some low-level callers install only the base Proposal schema. Do not pull
  ;; unknown attribute refs, and never require valid/full Git provenance here.
  (let [attributes (filterv #(contains? (:schema db) %) markers)]
    (boolean (and (seq attributes) (marked? (d/pull db attributes [:proposal/id id]))))))

(def ^:private upstream-schema
  [:map {:closed true} [:head :uuid] [:git-oid [:re #"^[0-9a-f]{40}$"]]])

(def ^:private cursor-schema
  [:map {:closed true} [:head :uuid] [:git-oid [:re #"^[0-9a-f]{40}$"]] [:proof-id :uuid]])

(defn- refuse! [reason]
  (throw (ex-info "Incoming Git provenance refused" {:type ::refused :reason reason})))

(defn incoming-identity [connection cursor incoming]
  (connections/connection! connection)
  (when-not (and (m/validate cursor-schema cursor) (m/validate upstream-schema incoming))
    (refuse! :invalid-upstream))
  {:version 1 :connection connection :cursor cursor :incoming incoming})

(defn incoming-key
  "The incoming revision is independent of a local autosave or a randomly
   generated merge commit. This identifies work, not a right to file/accept."
  [identity]
  (when-not (and (map? identity) (= #{:version :connection :cursor :incoming} (set (keys identity)))
                 (= 1 (:version identity))
                 (= identity (incoming-identity (:connection identity) (:cursor identity) (:incoming identity))))
    (refuse! :invalid-identity))
  (hasch/uuid [:simmis/github-incoming-proposal-v1 identity]))

(def ^:private root-schema
  [:map {:closed true} [:id :uuid] [:head :uuid] [:basis :map]])

(def ^:private provenance-schema
  [:map {:closed true} [:version [:= 1]] [:identity :map] [:key :uuid]
   [:producer [:map {:closed true} [:sync-id :uuid] [:claim :uuid] [:by :uuid]]]
   [:proposal [:map {:closed true} [:id :uuid] [:scope :uuid]
               [:review-ref [:re #"^refs/heads/github-review-[0-9a-f-]{36}$"]]
               [:base :uuid] [:head :uuid]]]
   [:canonical-basis :map]
   [:roots [:map {:closed true} [:base root-schema] [:local root-schema]
            [:incoming root-schema] [:candidate root-schema]]]])

(defn validate-provenance!
  "Immutable incoming evidence and frozen initial candidate are separate.
   Native retention, current authority and semantic certification are checked
   by the HOST; digest/shape correspondence alone supplies none of them."
  [body]
  (when-not (m/validate provenance-schema body) (refuse! :invalid-provenance))
  (let [{:keys [identity proposal roots canonical-basis]} body
        target (get-in identity [:connection :target])
        source-keys [:repository :store :backend]
        physical (select-keys target source-keys)]
    (streams/validate-basis! canonical-basis)
    (when-not (and (= (:key body) (incoming-key identity))
                   (= (:scope proposal) (:registry-system target))
                   (= (select-keys target [:repository :store :backend :branch :ref])
                      (select-keys canonical-basis [:repository :store :backend :branch :ref]))
                   (= (:head (:cursor identity)) (get-in roots [:base :head]))
                   (= (:head (:incoming identity)) (get-in roots [:incoming :head]))
                   (= (:base proposal) (get-in roots [:local :head]))
                   (= (:head proposal) (get-in roots [:candidate :head]))
                   (= canonical-basis (get-in roots [:local :basis])))
      (refuse! :provenance-mismatch))
    (doseq [[_ root] roots]
      (streams/validate-basis! (:basis root))
      (when-not (= physical (select-keys (:basis root) source-keys))
        (refuse! :physical-source-changed)))
    (when-not (= 4 (count (set (map :id (vals roots)))))
      (refuse! :root-id-reused)))
  (when (> (alength (.getBytes ^String (pr-str body) "UTF-8")) 32768)
    (refuse! :provenance-too-large))
  body)

(defn provenance-id [body]
  (validate-provenance! body)
  (hasch/uuid [:simmis/github-proposal-provenance-v1 body]))

(defn validate-proposal!
  "Check the complete ordinary single-repository filing against its retained
   provenance. Later resolution must not overwrite these frozen fork addresses."
  [body proposal]
  (validate-provenance! body)
  (let [expected (:proposal body) by (get-in body [:producer :by])
        forks (:proposal/forks proposal) fork (first forks)]
    (when-not (and (= (:id expected) (:proposal/id proposal))
                   (= (get-in body [:identity :connection :room]) (:proposal/room proposal))
                   (= by (:proposal/author proposal))
                   (= (:key body) (:proposal/github-key proposal))
                   (uuid? (:proposal/github-provenance proposal))
                   (= (provenance-id body) (:proposal/github-provenance-id proposal))
                   (not-any? #(contains? proposal %) [:proposal/run :proposal/adoption :proposal/review])
                   (vector? forks) (= 1 (count forks))
                   (= :repo (:proposal.fork/system-type fork))
                   (= (:scope expected) (:proposal.fork/scope fork))
                   (= (:proposal/room proposal) (:proposal.fork/authority-scope fork))
                   (= by (:proposal.fork/author fork))
                   (= (subs (:review-ref expected) 11) (:proposal.fork/branch fork))
                   (= (str (:base expected)) (:proposal.fork/base-commit fork))
                   (= (str (:head expected)) (:proposal.fork/head-commit fork)))
      (refuse! :proposal-mismatch)))
  proposal)
