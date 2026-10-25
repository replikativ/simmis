(ns is.simm.ops.capture-archive
  "Explicit HOST export of one inspected completed capture interval into its
   human owner's personal workspace. No scheduler, inference or shared export."
  (:require [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.effects :as effects]
            [dvergr.rooms.file-source :as file-source]
            [dvergr.rooms.files :as native-files]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.capture :as document]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.capture-sources :as sources]
            [is.simm.runtimes.media-cache :as cache]
            [is.simm.runtimes.screen-intake :as screen]
            [is.simm.runtimes.screen-recording :as recording]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.io FileInputStream]
           [java.util Arrays Base64]))

(defn- refuse! [reason]
  (throw (ex-info "Capture archive export refused" {:type ::refused :reason reason})))

(defn- personal! [binding]
  (let [room (d/entity (:control-db binding) [:room/id (:room-id binding)])]
    (when-not (and (= :personal-ai (:room/type room))
                   (= (:party binding) (:party/id (:room/owner room))))
      (refuse! :personal-owner-required))))

(defn- with-owned! [host principal handle f]
  (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (when-not (fn? with-operation) (refuse! :missing-lifetime))
    (with-operation
      (fn []
        (let [resolved (reads/resolve-host-write-room! host principal handle)
              room (:room resolved)]
          (binding [ec/*execution-context* (:execution-ctx host)]
            (preparation/with-host-operation!
              room
              (fn [lease]
                (preparation/with-host-current! lease (constantly true))
                (reads/validate-host-write-room! host principal handle resolved)
                (let [binding (reads/resolve-host-write! host principal handle)
                      authorize! (fn []
                                   (preparation/with-host-current! lease (constantly true))
                                   (reads/validate-host-write! host principal handle binding)
                                   (personal! (assoc binding :control-db (reads/current-host-control-db! host)))
                                   true)]
                  (when-not (identical? room (:room binding)) (refuse! :target-changed))
                  (personal! binding)
                  (clojure.core/binding [ec/*execution-context* (:ctx room)]
                    (f binding authorize!)))))))))))

(defn- ids! [session segment]
  (when-not (and (uuid? session) (integer? segment) (<= 0 segment 999999))
    (refuse! :invalid-selection)))

(defn- selected! [owner session index]
  (let [source (recording/archive-selection! owner session index)
        start (get-in source [:session :started-at])
        from (+ start (get-in source [:segment :offset-ms]))
        until (+ from (get-in source [:segment :duration-ms]))
        ;; Clip the inclusive legacy query BEFORE its limit. Multiple rows at
        ;; the next boundary must not hide an overflow inside this interval.
        frames (->> (screen/search-frames owner "" 33 [{:from from :until (dec until)}])
                    (sort-by (juxt :at :blob-id)) vec)]
    (when (> (count frames) 32) (refuse! :too-many-observations))
    (when-not (every? #(and (integer? (:at %)) (<= from (:at %)) (< (:at %) until)) frames)
      (refuse! :invalid-observation-time))
    {:source source :frames frames
     :basis (hasch/uuid [:simmis/archive-evidence-v1 (:revision source) frames])}))

(defn- plan! [resource owner sid index]
  (let [{:keys [source frames basis]} (selected! owner sid index)
        inspected (sources/inspect-file! (assoc source :size (get-in source [:segment :size])))
        {:keys [observations]}
        (reduce (fn [{:keys [remaining observations]} frame]
                  (let [image (sources/stage-image! resource (:blob-id frame) remaining)
                        id (hasch/uuid [:simmis/screenshot-source-v1 owner sid (:at frame) (:blob-id frame)])
                        observation (merge image (select-keys frame [:at :model]) {:id id}
                                           (if (:failed frame) {:failed (:failed frame)} {:text (:text frame)}))]
                    {:remaining (- remaining (:size image)) :observations (conj observations observation)}))
                {:remaining sources/image-total-limit :observations []} frames)
        note (document/observation-note (:session source)
                                        (assoc (:segment source) :content (:content inspected)) observations)
        ;; The formatter already bounds actual UTF-8 to 1.5 MB. Also reject
        ;; malformed Unicode with the native document codec before any claim.
        _ (markdown/source-bytes (:text note))
        snapshot (hasch/uuid [:simmis/completed-capture-preview-v1 basis (:text note) (:assets note)])]
    {:source inspected :basis basis :note note :snapshot snapshot}))

(defn preview!
  "Authenticated HOST preview. Descriptors and text only; no File, connection,
   borrowed source or permission escapes. No inference/publication is performed."
  [host principal handle {:keys [session segment] :as request}]
  (when-not (and (map? request) (= #{:session :segment} (set (keys request)))) (refuse! :invalid-selection))
  (ids! session segment)
  (with-owned! host principal handle
    (fn [binding authorize!]
      (let [plan (plan! (:media-cache host) (:party binding) session segment)]
        (authorize!)
        {:snapshot (:snapshot plan) :target-stamp (:target-stamp binding)
         :session session :segment segment :note (:note plan)}))))

(defn- boundary [room authorize!]
  (let [base (native-files/host-boundary room (effects/make-sink))]
    (fn [] (update (base) :handlers conj (fn [effect next] (authorize!) (next effect))))))

(defn- positive! [completion path expected]
  (let [result (:result completion)]
    (when-not (and (= :returned (:completion completion)) (nil? (:error completion))
                   (true? (:applied? result))
                   (= [(str "/" path)] (mapv :path (:written result)))
                   (= expected (get-in result [:written 0 :entry]))
                   (empty? (:removed result)) (some? (:commit result)))
      (refuse! :native-publication-pending))
    {:outcome :created :path path :entry expected :commit (:commit result)}))

(defn- verify-existing! [conn path expected file]
  (when-not (= expected (files/entry @conn path)) (refuse! :destination-conflict))
  (with-open [in (FileInputStream. file)]
    (content/consume-by-id!
     conn (:content expected)
     (fn [chunk]
       (let [actual (.readNBytes in (alength ^bytes chunk))]
         (when-not (Arrays/equals ^bytes actual ^bytes chunk) (refuse! :destination-conflict)))))
    (when-not (= -1 (.read in)) (refuse! :destination-conflict)))
  {:outcome :existing-verified :path path :entry expected})

(defn publish!
  "One inspected closed interval. A pending operation stops the batch and
   retains its claim; never infer an earlier ACK or automatically retry it."
  [host principal handle {:keys [session segment snapshot expected-target-stamp] :as request}]
  (when-not (and (map? request) (= #{:session :segment :snapshot :expected-target-stamp} (set (keys request)))
                 (uuid? snapshot) (uuid? expected-target-stamp)) (refuse! :invalid-selection))
  (ids! session segment)
  (with-owned! host principal handle
    (fn [binding authorized!]
      (when-not (= expected-target-stamp (:target-stamp binding)) (refuse! :target-changed))
      (recording/assert-unclaimed-archive! (:party binding) session segment)
      (let [resource (:media-cache host) owner (:party binding)
            {:keys [source basis note] :as plan} (plan! resource owner session segment)]
        (when-not (= snapshot (:snapshot plan)) (refuse! :selection-changed))
        (when (files/entry @(:conn binding) (:path note)) (refuse! :note-already-exists))
        (authorized!)
        (let [claim (recording/claim-archive! source snapshot expected-target-stamp (:room-id binding) (:path note))
              authorize! (fn []
                           (authorized!)
                           (recording/validate-archive-claim! claim)
                           (when-not (= basis (:basis (selected! owner session segment))) (refuse! :selection-changed))
                           true)
              room (:room binding) system (:system binding)
              publish-asset!
              (fn [{:keys [path content size] :as asset}]
                (let [expected {:content content :size size :mode repo/default-mode}
                      consume! (fn [file]
                                 (authorize!)
                                 (let [result (if (files/entry @(:conn binding) path)
                                                (verify-existing! (:conn binding) path expected file)
                                                (file-source/with-finalized-file!
                                                  file
                                                  #(positive!
                                                    (native-files/write-file-to-completion!
                                                     room system path %
                                                     {:expected-worktree {(str "/" path) nil}
                                                      :boundary (boundary room authorize!)
                                                      :message (str "Archive capture " path)}) path expected)))]
                                   (authorize!)
                                   result))]
                  (if (= :video (:kind asset))
                    (sources/with-staged-file! resource source consume!)
                    ;; plan! materialized this exact image. A missing cache hit
                    ;; must refuse rather than falling back to an unbound read.
                    (cache/with-materialized-file! resource content size (fn [_] (refuse! :missing-staged-image)) consume!))))
              assets (mapv publish-asset! (:assets note))
              _ (authorize!)
              bytes (.getBytes ^String (:text note) "UTF-8")
              expected {:content (blob/blob-id bytes) :size (alength bytes) :mode repo/default-mode}
              path (:path note)
              written (positive! (native-files/write-to-completion!
                                  room system {(str "/" path) {:base64 (.encodeToString (Base64/getEncoder) bytes)}}
                                  {:expected-worktree {(str "/" path) nil} :boundary (boundary room authorize!)
                                   :message (str "Archive capture observations " path)}) path expected)]
          (authorize!)
          (let [finished (recording/finish-archive! claim (:content expected))]
            {:snapshot snapshot :target-stamp expected-target-stamp :note written :assets assets
             :recording finished}))))))
