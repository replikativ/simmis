(ns is.simm.runtimes.transcripts-test
  "Transcripts: jigasi files and transcribed audio both become a drive file
   under transcripts/ plus one room message; transcription is explicit, chunks
   long audio, and says why when it fails. Speech-to-text is mocked here — no
   model, no network; the real faster-whisper path is exercised by hand (see
   the PR)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dvergr.audio.stt :as stt]
            [taoensso.telemere :as log]
            [is.simm.agents.room-agents :as ra]
            [is.simm.model.drives :as drives]
            [is.simm.model.rooms :as rooms]
            [is.simm.runtimes.transcripts :as tr]))

(def ^:private fixture "test/fixtures/tone-1s.wav")

(defn- fixture-bytes ^bytes [] (java.nio.file.Files/readAllBytes (.toPath (io/file fixture))))

(defn- with-room
  "Stub the room plumbing: a live room, its drive (an atom of
   {path {:bytes :mime}}), and the room-log message sink."
  [f]
  (let [room (random-uuid) author (random-uuid)
        drive (atom {"call.wav" {:bytes (fixture-bytes) :mime "audio/wav"}
                     "notes.txt" {:bytes (.getBytes "hi") :mime "text/plain"}})
        posted (atom [])
        node (fn [path] (when-let [{:keys [mime bytes]} (get @drive path)]
                          {:fs.node/id (java.util.UUID/nameUUIDFromBytes (.getBytes ^String path))
                           :fs.node/name (last (str/split path #"/"))
                           :fs.node/kind :file :fs.node/mime mime
                           :fs.node/size (alength ^bytes bytes)
                           ::path path}))]
    (with-redefs [ra/live-room (fn [r] (when (= r room) {:ctx nil}))
                  drives/room-conn (constantly ::conn)
                  drives/resolve-path (fn [_ p] (node p))
                  drives/stat (fn [_ id] (some #(when (= id (:fs.node/id %)) %) (map node (keys @drive))))
                  drives/read-file (fn [_ id] (some (fn [p] (when (= id (:fs.node/id (node p)))
                                                              (:bytes (get @drive p))))
                                                    (keys @drive)))
                  drives/store-in-room! (fn [r dir name bytes & {:keys [mime]}]
                                          (assert (= r room))
                                          (swap! drive assoc (str (str/join "/" dir) "/" name)
                                                 {:bytes bytes :mime mime}))
                  ra/post-room-record! (fn [r who content kind]
                                         (swap! posted conj {:room r :author who
                                                             :content content :kind kind})
                                         (random-uuid))]
      (f {:room room :author author :drive drive :posted posted :node node}))))

(deftest a-transcribed-file-becomes-a-drive-document-and-one-room-message
  (with-room
    (fn [{:keys [room author drive posted]}]
      (let [heard (atom [])]
        (with-redefs [stt/transcribe (fn [{:keys [bytes mime]}]
                                       (swap! heard conj [(alength ^bytes bytes) mime])
                                       "we push the launch to March")]
          (let [{:keys [path]} (tr/transcribe-drive-file! room author "/drive/call.wav")]
            (testing "stt got the file's bytes, whole (it is small)"
              (is (= [[(alength (fixture-bytes)) "audio/wav"]] @heard)))
            (testing "the transcript is a Markdown document under transcripts/"
              (is (re-matches #"transcripts/\d{4}-\d{2}-\d{2}-\d{4}-call\.md" path))
              (let [md (String. ^bytes (:bytes (get @drive path)) "UTF-8")]
                (is (str/starts-with? md "# call"))
                (is (str/includes? md "we push the launch to March"))
                (is (str/includes? md "transcribed from /drive/call.wav"))))
            (testing "and ONE room-log message, authored by the asker, kind :transcript"
              (is (= 1 (count @posted)))
              (let [{:keys [content kind] who :author} (first @posted)]
                (is (= :transcript kind))
                (is (= author who))
                (is (str/includes? content (str "/drive/" path)))
                (is (str/includes? content "we push the launch to March"))))))))))

(deftest only-audio-is-transcribed-and-failures-say-why
  (with-room
    (fn [{:keys [room author posted]}]
      (testing "a non-audio file is refused before any stt call"
        (with-redefs [stt/transcribe (fn [_] (throw (ex-info "must not be called" {})))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not an audio file"
                                (tr/transcribe-drive-file! room author "notes.txt")))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No file"
                                (tr/transcribe-drive-file! room author "missing.wav")))))
      (testing "no backend producing text is an error naming the file"
        (with-redefs [stt/transcribe (constantly nil)]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No speech-to-text backend.*call.wav"
                                (tr/transcribe-drive-file! room author "call.wav")))))
      (testing "in the background, a failure is told to the asker IN the room"
        (with-redefs [stt/transcribe (constantly nil)]
          @(tr/transcribe-in-background! room author "call.wav" "call.wav")
          (let [{:keys [kind content] who :author} (last @posted)]
            (is (= :transcript-failed kind))
            (is (= author who))
            (is (str/includes? content "Transcription of call.wav failed"))))))))

(deftest node-ids-work-as-well-as-paths
  (with-room
    (fn [{:keys [room author node]}]
      (with-redefs [stt/transcribe (constantly "hello")]
        (is (:path (tr/transcribe-drive-file! room author (:fs.node/id (node "call.wav")))))))))

(deftest long-audio-is-cut-into-chunks-in-order
  (let [sent (atom [])]
    (with-redefs [tr/chunk-threshold-bytes 10
                  tr/ffmpeg-split! (fn [_ dir]
                                     (vec (for [i [0 1 2]]
                                            (doto (io/file dir (str "chunk-000" i ".mp3"))
                                              (spit (str "part" i))))))
                  stt/transcribe (fn [{:keys [bytes mime]}]
                                   (swap! sent conj mime)
                                   (str "text of " (String. ^bytes bytes "UTF-8")))]
      (is (= "text of part0\n\ntext of part1\n\ntext of part2"
             (tr/transcribe-bytes (fixture-bytes) "audio/wav" "long.wav")))
      (is (= ["audio/mpeg" "audio/mpeg" "audio/mpeg"] @sent)))))

(deftest ffmpeg-really-splits
  (if (zero? (:exit (try (clojure.java.shell/sh "ffmpeg" "-version")
                         (catch java.io.IOException _ {:exit 1}))))
    (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                        "split" (make-array java.nio.file.attribute.FileAttribute 0)))]
      (with-redefs [tr/chunk-seconds 0.4]
        (let [chunks (#'tr/ffmpeg-split! (io/file fixture) dir)]
          (is (<= 2 (count chunks)) "a 1 s file in 0.4 s pieces")
          (is (every? #(pos? (.length ^java.io.File %)) chunks)))))
    (log/log! {:level :warn :id ::ffmpeg-missing
               :msg "ffmpeg not installed — the real split is not exercised"})))

(deftest jigasi-files-parse-and-map-to-their-room
  (let [room (random-uuid)]
    (testing "txt: the room uuid comes from the path or the content"
      (is (= (str room) (:room-name (tr/parse-txt-transcript (str "/t/" room ".txt") "hello"))))
      (is (nil? (tr/parse-txt-transcript "/t/x.txt" "no room here"))))
    (testing "json: speech events, attributed"
      (let [t (tr/parse-json-transcript
               (str "{\"room_name\":\"" room "\",\"events\":["
                    "{\"event\":\"SPEECH\",\"participant\":{\"name\":\"Ada\"},"
                    "\"transcript\":[{\"text\":\"we ship in March\"}]},"
                    "{\"event\":\"JOIN\",\"participant\":{\"name\":\"Bob\"}}]}"))]
        (is (= (str room) (:room-name t)))
        (is (= [{:name "Ada" :text "we ship in March"}] (:lines t)))))
    (testing "an unknown room is skipped, a known one is posted by its creator"
      (let [creator (random-uuid)
            f (doto (java.io.File/createTempFile "call" ".txt")
                (spit (str "Transcript of " room "\nAda: hello")))
            got (atom nil)]
        (with-redefs [rooms/get-room (fn [r] (when (= r room)
                                               {:room/name "Planning" :room/created-by creator}))
                      tr/post-transcript! (fn [r who body opts] (reset! got [r who body opts]) :posted)]
          (is (= :posted (tr/ingest-file! f)))
          (let [[r who body opts] @got]
            (is (= room r))
            (is (= creator who))
            (is (str/includes? body "Ada: hello"))
            (is (= "Planning call" (:title opts))))
          (is (= :unmapped (tr/ingest-file! (doto (java.io.File/createTempFile "nouuid" ".txt")
                                              (spit "no uuid"))))))))))

(deftest the-watcher-posts-new-files-not-old-ones
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "jigasi" (make-array java.nio.file.attribute.FileAttribute 0)))
        old (doto (io/file dir "old.txt") (spit "already here"))
        seen (promise)]
    (with-redefs [tr/ingest-file! (fn [f] (deliver seen (.getName ^java.io.File f)) :ok)]
      (try
        (is (= :started (tr/start! (str dir))))
        (spit (io/file dir "new.txt") "fresh")
        ;; the bound only keeps a broken watcher from hanging the suite
        (is (= "new.txt" (deref seen 20000 ::never)))
        (finally (tr/stop!))))
    (is (.exists old))))

(deftest no-directory-means-no-watcher
  (is (nil? (tr/start! nil)))
  (is (nil? (tr/start! ""))))
