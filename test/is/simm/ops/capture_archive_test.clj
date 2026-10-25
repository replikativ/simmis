(ns is.simm.ops.capture-archive-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.drive.blobs :as blobs]
            [dvergr.rooms.files :as native]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.ops.capture-archive :as archive]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.runtimes.capture-sources :as sources]
            [is.simm.runtimes.media-cache :as cache]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.runtimes.screen-intake :as screen]
            [is.simm.runtimes.screen-recording :as recording])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- with-fixture
  ([f] (with-fixture true f))
  ([owned-first? f]
   (@#'fixture/with-fixture
    owned-first?
    (fn [{:keys [w conn] :as data}]
      (let [owner (lifetime/create) resource (cache/create)
            cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? false}
            _ (d/create-database cfg) recordings (d/connect cfg)
            file (.toFile (Files/createTempFile "capture-export-" ".webm" (make-array FileAttribute 0)))
            sid (random-uuid) bytes (.getBytes "recording" "UTF-8")
            image (byte-array [(unchecked-byte 255) (unchecked-byte 216) (unchecked-byte 255) 0 0])
            image-id (blobs/sha256-hex image)
            frames (atom [{:at 1100 :blob-id image-id :text "The customer requested a revised proposal." :model "fixture-model"}])
            select recording/archive-selection!]
        (lifetime/open! owner)
        (try
          (spit file "recording")
          (d/transact conn [{:room/id (:room w) :room/type :personal-ai}])
          (d/transact recordings recording/recording-schema)
          (d/transact recordings [{:recording.session/id sid :recording.session/started-at 1000
                                   :recording.session/ended-at 5000 :recording.session/mime "video/webm"}
                                  {:recording.segment/session [:recording.session/id sid] :recording.segment/idx 0
                                   :recording.segment/status :ready :recording.segment/offset-ms 10
                                   :recording.segment/duration-ms 1000 :recording.segment/bytes 9
                                   :recording.segment/blob-id (blobs/sha256-hex bytes)}])
          (with-redefs-fn {(var recording/conn-for) (constantly recordings)
                           (var recording/archive-selection!) (fn [owner session idx] (assoc (select owner session idx) :file file))
                           (var screen/search-frames) (fn [party _ n windows]
                                                        (is (= (:owner w) party))
                                                        (let [{:keys [from until]} (first windows)]
                                                          (->> @frames (filter #(<= from (:at %) until))
                                                               (sort-by :at >) (take n) vec)))
                           (var blobs/get-bytes-bounded) (fn [id limit]
                                                           (is (= image-id id)) (is (<= (alength image) limit)) (aclone image))}
            #(f (-> data
                    (assoc :sid sid :frames frames :recordings recordings :capture-owner owner :cache resource :image-bytes image)
                    (update :host assoc :media-cache resource)
                    (update-in [:host :config :host-lifecycle] assoc :with-operation (fn [work] (lifetime/with-operation! owner work))))))
          (finally
            (lifetime/fence! owner)
            (lifetime/join! owner 5000)
            (cache/close! resource)
            (d/release recordings) (d/delete-database cfg)
            (.delete file))))))))

(defn- preview [{:keys [host w sid]}]
  (archive/preview! host (ops/party-principal (:owner w)) (str (:room w)) {:session sid :segment 0}))
(defn- publish [{:keys [host w sid]} p]
  (archive/publish! host (ops/party-principal (:owner w)) (str (:room w))
                    {:session sid :segment 0 :snapshot (:snapshot p) :expected-target-stamp (:target-stamp p)}))

(deftest completed-export-publishes-assets-before-source-preserving-observation-note
  (with-fixture
    (fn [{:keys [owned attached] :as data}]
      (let [p (preview data) before @owned other @attached path (get-in p [:note :path])]
        (is (= before @owned) "Preview stages bytes privately but writes no workspace files")
        (let [result (publish data p) text (:text (files/read-file-in @owned path))]
          (is (= :created (get-in result [:note :outcome])))
          (is (= [:created :created] (mapv :outcome (:assets result))))
          (is (= :complete (get-in result [:recording :state])))
          (is (= (get-in p [:note :text]) text))
          (is (str/includes? text "../images/"))
          (is (str/includes? text "The customer requested a revised proposal.")))
        (is (= other @attached))
        (is (thrown? Exception (publish data p)))))))

(deftest changed-observation-and-shared-room-refuse-before-publication
  (with-fixture
    (fn [{:keys [owned frames conn w] :as data}]
      (let [p (preview data) before @owned]
        (swap! frames update-in [0 :text] str " Changed.")
        (is (thrown? Exception (publish data p)))
        (is (= before @owned))
        (d/transact conn [{:room/id (:room w) :room/type :group}])
        (is (thrown? Exception (preview data)))
        (is (= before @owned))))))

(deftest uncertain-native-write-retains-claim-and-never-publishes-the-note
  (with-fixture
    (fn [{:keys [owned] :as data}]
      (let [p (preview data) before @owned calls (atom 0)]
        (with-redefs [native/write-file-to-completion! (fn [& _] (swap! calls inc) {:completion :pending})]
          (is (thrown? Exception (publish data p)))
          (is (thrown? Exception (publish data p))))
        (is (= 1 @calls))
        (is (= before @owned))))))

(deftest identical-existing-assets-are-byte-verified-without-inferring-old-acknowledgements
  (with-fixture
    (fn [{:keys [owned image-bytes] :as data}]
      (let [p (preview data) assets (get-in p [:note :assets])]
        (doseq [asset assets]
          (repo/write! owned (:path asset) (if (= :video (:kind asset)) (.getBytes "recording" "UTF-8") image-bytes)))
        (repo/stage-all! owned)
        (repo/commit! owned {:message "Existing fixture assets"})
        (with-redefs [native/write-file-to-completion! (fn [& _] (throw (AssertionError. "existing asset must not be rewritten")))]
          (let [result (publish data p)]
            (is (= [:existing-verified :existing-verified] (mapv :outcome (:assets result))))
            (is (every? #(not (contains? % :commit)) (:assets result)))
            (is (= :created (get-in result [:note :outcome])))))))))

(deftest note-is-never-published-after-revocation-or-owner-change-during-assets
  (doseq [change [:owner :default]]
    (with-fixture
      (fn [{:keys [conn w owned attached-descriptor] :as data}]
        (let [p (preview data) path (get-in p [:note :path]) write native/write-file-to-completion!]
          (with-redefs [native/write-file-to-completion!
                        (fn [& args]
                          (let [result (apply write args)]
                            (d/transact conn [(case change
                                                :owner {:room/id (:room w) :room/owner [:party/id (:member w)]}
                                                :default {:room/id (:room w) :room/default-repository
                                                          [:system/id (:registry-system attached-descriptor)]})])
                            result))]
            (is (thrown? Exception (publish data p))))
          (is (nil? (files/entry @owned path))))))))

(deftest canonical-target-does-not-depend-on-system-registration-order
  (with-fixture false
    (fn [{:keys [owned attached] :as data}]
      (let [p (preview data) other @attached result (publish data p)]
        (is (= :created (get-in result [:note :outcome])))
        (is (some? (files/entry @owned (get-in p [:note :path]))))
        (is (= other @attached))))))

(deftest repeated-claim-refuses-before-any-source-io
  (with-fixture
    (fn [data]
      (let [p (preview data) reads (atom 0)]
        (with-redefs [native/write-file-to-completion! (fn [& _] {:completion :pending})]
          (is (thrown? Exception (publish data p))))
        (with-redefs [sources/inspect-file! (fn [& _] (swap! reads inc))
                      sources/stage-image! (fn [& _] (swap! reads inc))]
          (is (thrown? Exception (publish data p))))
        (is (zero? @reads))))))

(deftest revoked-permission-at-native-gate-never-publishes-an-asset
  (with-fixture
    (fn [{:keys [w conn owned] :as data}]
      (let [p (preview data) before @owned write native/write-file-to-completion!]
        (with-redefs [native/write-file-to-completion!
                      (fn [& args]
                        (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner]
                                          [:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]])
                        (apply write args))]
          (is (thrown? Exception (publish data p))))
        (is (= before @owned))))))

(deftest app-cleanup-waits-for-the-original-synchronous-cache-borrow
  (with-fixture
    (fn [{:keys [capture-owner] :as data}]
      (let [p (preview data) entered (promise) release (promise)
            staged sources/with-staged-file! worker (atom nil)]
        (try
          (with-redefs [sources/with-staged-file!
                        (fn [resource source consume!]
                          (staged resource source
                                  (fn [file] (deliver entered file) @release (consume! file))))]
            (reset! worker (future (publish data p)))
            (let [file (deref entered 10000 ::timeout)]
              (is (instance? java.io.File file))
              (lifetime/fence! capture-owner)
              (is (thrown? Exception (lifetime/join! capture-owner 0)))
              (when (instance? java.io.File file) (is (.exists ^java.io.File file)))
              (deliver release true)
              (let [result (deref @worker 10000 ::timeout)]
                (is (= :complete (get-in result [:recording :state])))
                (is (not-any? #(instance? java.io.File %) (tree-seq coll? seq result))))))
          (finally
            (deliver release true)
            (when-let [running @worker] @running)))))))

(deftest observation-and-encoded-note-overflow-refuse-before-claims-or-writes
  (doseq [overflow [:frames :utf8]]
    (with-fixture
      (fn [{:keys [frames owned recordings] :as data}]
        (let [before @owned frame (first @frames)
              count (if (= overflow :frames) 33 12)
              text (if (= overflow :utf8) (apply str (repeat 100000 "λ")) "observation")]
          (reset! frames (mapv #(assoc frame :at (+ 1100 %) :text text) (range count)))
          (is (thrown? Exception (preview data)))
          (is (= before @owned))
          (is (empty? (d/q '[:find [?claim ...] :where [_ :recording.segment/archive-claim ?claim]] @recordings))))))))

(deftest many-observations-at-next-boundary-cannot-hide-interior-overflow
  (with-fixture
    (fn [{:keys [frames owned] :as data}]
      (let [frame (first @frames) before @owned]
        (reset! frames (vec (concat (repeat 34 (assoc frame :at 2010))
                                    (map #(assoc frame :at (+ 1100 %)) (range 33)))))
        (is (thrown? Exception (preview data)))
        (is (= before @owned))))))

(deftest mismatched-existing-asset-is-never-overwritten
  (with-fixture
    (fn [{:keys [owned] :as data}]
      (let [p (preview data) path (get-in p [:note :assets 0 :path])]
        (repo/write! owned path (.getBytes "someone else's content" "UTF-8"))
        (repo/stage-all! owned) (repo/commit! owned {:message "Conflicting fixture asset"})
        (let [before @owned]
          (is (thrown? Exception (publish data p)))
          (is (= before @owned))
          (is (nil? (files/entry @owned (get-in p [:note :path])))))))))

(deftest lost-note-completion-never-finishes-or-replays-the-export
  (with-fixture
    (fn [{:keys [owned recordings] :as data}]
      (let [p (preview data) write native/write-to-completion!]
        (with-redefs [native/write-to-completion!
                      (fn [& args] (apply write args) {:completion :pending})]
          (is (thrown? Exception (publish data p))))
        (is (some? (files/entry @owned (get-in p [:note :path]))))
        (is (= #{:pending} (set (d/q '[:find [?state ...] :where [_ :recording.segment/archive-state ?state]] @recordings))))
        (with-redefs [native/write-file-to-completion! (fn [& _] (throw (AssertionError. "must not replay")))
                      native/write-to-completion! (fn [& _] (throw (AssertionError. "must not replay")))]
          (is (thrown? Exception (publish data p))))))))
