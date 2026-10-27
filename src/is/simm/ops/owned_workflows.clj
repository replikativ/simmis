(ns is.simm.ops.owned-workflows
  "Inactive fixed HOST workflow reads. Public data and ephemeral read context
   are separate; no transport, writer, selected fallback or execution authority."
  (:require [hasch.core :as hasch]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]))

(def ^:private uuid-string [:string {:min 36 :max 36}])
(def ^:private list-schema
  [:map {:closed true}
   [:offset {:optional true} [:int {:min 0 :max workflows/max-list-entries}]]
   [:expected-revision {:optional true} uuid-string]])
(def ^:private details-schema
  [:map {:closed true} [:name workflows/name-schema]
   [:expected-revision {:optional true} uuid-string]])
(def ^:private source-schema
  [:map {:closed true} [:name workflows/name-schema]
   [:file-offset {:optional true} [:int {:min 0 :max workflows/max-files}]]
   [:offset {:optional true} [:int {:min 0 :max workflows/max-file-bytes}]]
   [:path {:optional true} [:string {:min 1 :max 512}]]
   [:expected-revision {:optional true} uuid-string]])

(defn- invalid! []
  (throw (ex-info "Invalid owned workflow read" {:type ::invalid})))

(defn- request! [schema request]
  (when-not (m/validate schema request) (invalid!))
  (when (and (contains? request :name)
             (not (re-matches #"[a-z0-9][a-z0-9-]{0,63}" (:name request))))
    (invalid!))
  (when-let [expected (:expected-revision request)]
    (or (parse-uuid expected) (invalid!))))

(defn- interrupted? [failure]
  (loop [cause failure seen #{}]
    (cond (nil? cause) false
          (instance? InterruptedException cause) true
          (contains? seen cause) false
          :else (recur (.getCause ^Throwable cause) (conj seen cause)))))

(defn- storage! [f]
  (try (f)
       (catch Throwable error
         (when (interrupted? error) (.interrupt (Thread/currentThread)))
         ;; Never promote arbitrary helper tags/messages/causes into transport
         ;; status or diagnostics. Only NORMAL metadata comparisons conflict.
         (throw (ex-info "Owned workflow read unavailable" {:type ::unavailable})))))

(defn- expected! [expected actual]
  (when (and expected (not= expected actual))
    (throw (ex-info "Owned workflow source changed; reload metadata" {:type ::conflict}))))

(defn- envelope [binding data]
  {:data data :read-context (select-keys binding [:control-db :party :room-id])})

(defn source-id
  "HOST pure target-bound identity over a validated manifest; not authority."
  [binding name manifest]
  (hasch/uuid [:simmis/owned-workflow-source-v1 (:room-id binding) (:target-stamp binding) name manifest]))

(defn details-of
  "HOST pure safe formatting of ORIGINAL captured source, including write ACKs."
  [binding name snapshot]
  (let [revision (str (source-id binding name (:manifest snapshot)))
        details (workflows/details-of name snapshot)]
    (-> (cond-> details
          (seq (get-in details [:inspection :problems]))
          (update-in [:inspection :problems]
                     (fn [problems]
                       (mapv #(if (contains? #{"workflow.edn is missing."
                                              "workflow.edn is not valid EDN."
                                              "workflow.edn needs a title, task and supported definition fields."
                                              "Ordinary workflow parameters must use unqualified names and literal string values."
                                              "Workflow source has invalid definition, fixture or evaluation metadata."} %)
                                % "Workflow source is incomplete or invalid; inspect its files")
                             (take 10 problems)))))
        (assoc :revision revision)
        (assoc-in [:inspection :revision] revision))))

(defn- captured-details! [host principal handle request expected]
  (let [binding (storage! #(reads/resolve-host-read! host principal handle))
        name (:name request)
        manifest (storage! #(workflows/manifest-in (:db binding) name))
        revision (storage! #(source-id binding name manifest))]
    (expected! expected revision)
    ;; These are NORMAL checks over the already validated bounded manifest,
    ;; not exception-tag classification. Refuse invalid pages before bodies.
    (if-let [path (:path request)]
      (let [entry (first (filter #(= path (:path %)) manifest))]
        (when-not (and entry (<= (get request :offset 0) (:size entry))) (invalid!)))
      (when (> (get request :file-offset 0) (count manifest)) (invalid!)))
    (let [details (storage! #(details-of binding name (workflows/snapshot-in (:db binding) name)))]
      {:binding binding
       :details details})))

(defn details!
  "Full captured source for a host editor. :read-context stays HOST-only and
   must never be serialized, persisted or reused as a permission lease."
  [host authenticated-human-principal original-room-handle request]
  (let [expected (request! details-schema request)
        {:keys [binding details]} (captured-details! host authenticated-human-principal original-room-handle request expected)]
    (envelope binding details)))

(defn source!
  "Bounded metadata/chunks of the SAME owned captured source. Expected target-
   bound revision is compared BEFORE loading bodies; no executable bundle ID."
  [host authenticated-human-principal original-room-handle request]
  (let [expected (request! source-schema request)]
    (when (and (or (:path request) (pos? (get request :file-offset 0))) (nil? expected))
      (invalid!))
    (let [{:keys [binding details]} (captured-details! host authenticated-human-principal original-room-handle request expected)
          request (cond-> request expected (assoc :expected-revision (:revision details)))]
      (envelope binding (storage! #(workflows/source-page-of details request))))))

(defn list!
  "Body-free names with a target-bound revision. Check identity before paging
   so a stale now-out-of-range page reports conflict, not a projection failure."
  [host authenticated-human-principal original-room-handle request]
  (let [expected (request! list-schema request)
        binding (storage! #(reads/resolve-host-read! host authenticated-human-principal original-room-handle))
        first-page (storage! #(workflows/list-page-in (:db binding) {}))
        revision (storage! #(hasch/uuid [:simmis/owned-workflow-list-v1 (:room-id binding) (:target-stamp binding)
                                         (parse-uuid (:revision first-page))]))]
    (expected! expected revision)
    (when (> (get request :offset 0) (:total first-page)) (invalid!))
    (let [page (if (zero? (get request :offset 0)) first-page
                   (storage! #(workflows/list-page-in (:db binding) (select-keys request [:offset]))))]
      (envelope binding (assoc page :revision (str revision))))))
