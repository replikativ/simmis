(ns is.simm.runtimes.media-cache
  "Application-owned, disposable media materializations for http-kit's ByteBuffer
   response body. Cache hits supply bytes, never authorization. Not room state."
  (:require [datahike.blob :as blob])
  (:import [java.io FileInputStream FileOutputStream FilterOutputStream]
           [java.nio ByteBuffer]
           [java.nio.channels FileChannel FileChannel$MapMode]
           [java.nio.file Files Path StandardOpenOption OpenOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(def policy {:max-file-bytes Integer/MAX_VALUE
             :max-bytes (* 8 1024 1024 1024) :max-entries 4096})

(defn- refuse! [reason]
  (throw (ex-info "Media bytes are unavailable" {:type ::refused :reason reason})))

(defn create
  "No physical directory or worker is created until the first materialization.
   Fixed aggregate limits apply until close; queued buffers prevent safe LRU
   accounting with the pinned HTTP adapter's completion semantics."
  ([] (create policy))
  ([limits]
   (when-not (and (map? limits) (= (set (keys policy)) (set (keys limits)))
                  (every? #(and (integer? %) (pos? %) (<= % Long/MAX_VALUE)) (vals limits))
                  (<= (:max-file-bytes limits) Integer/MAX_VALUE)
                  (<= (:max-file-bytes limits) (:max-bytes limits)))
     (refuse! :invalid-policy))
   {:guard (Object.) :limits limits
    :state (atom {:closed? false :entries {} :bytes 0 :paths #{} :directory nil})}))

(defn- check-interruption! []
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "Media materialization interrupted"))))

(defn- directory! [{:keys [state]}]
  (or (:directory @state)
      (let [path (Files/createTempDirectory
                  "simmis-media-"
                  (into-array FileAttribute
                              [(PosixFilePermissions/asFileAttribute
                                (PosixFilePermissions/fromString "rwx------"))]))]
        (swap! state assoc :directory path)
        path)))

(defn- remove-path! [state path]
  (Files/deleteIfExists path)
  (swap! state update :paths disj path))

(defn- bounded-output [out size]
  (let [written (volatile! 0)
        reserve! (fn [n]
                   (check-interruption!)
                   (when-not (<= 0 n (- size @written)) (refuse! :size-mismatch))
                   (vswap! written + n))]
    (proxy [FilterOutputStream] [out]
      (write
        ([value]
         (if (bytes? value)
           (do (reserve! (alength ^bytes value)) (.write out ^bytes value))
           (do (reserve! 1) (.write out (int value)))))
        ([buffer offset length]
         (reserve! length)
         (.write out ^bytes buffer (int offset) (int length)))))))

(defn materialize!
  "Synchronous owned extent. `copy!` must write exact logical content into the
   supplied OutputStream while its caller retains App/Room/store lifetime.
   Copies and hashes use bounded buffers. A successful result contains an
   independent read-only buffer position; no native database handle escapes.

   Files are private and immutable. This implementation requires POSIX temp-file
   permissions; cleanup uses Unix mapped-file unlink semantics. No force-unmap,
   truncation, source-path exposure or permission inference from content IDs."
  [{:keys [guard state limits] :as cache} content size copy!]
  (when-not (and (uuid? content) (integer? size) (<= 0 size (:max-file-bytes limits)) (fn? copy!))
    (refuse! :invalid-content))
  (locking guard
    (check-interruption!)
    (when (:closed? @state) (refuse! :closed))
    (if-let [entry (get-in @state [:entries content])]
      (do (when-not (= size (:size entry)) (refuse! :size-mismatch))
          {:size size :buffer (.asReadOnlyBuffer ^ByteBuffer (:buffer entry))})
      (do
        (when (or (>= (count (:entries @state)) (:max-entries limits))
                  (> size (- (:max-bytes limits) (:bytes @state))))
          (refuse! :capacity))
        (let [directory (directory! cache)
              path (Files/createTempFile ^Path directory "content-" ".bin"
                                         (into-array FileAttribute
                                                     [(PosixFilePermissions/asFileAttribute
                                                       (PosixFilePermissions/fromString "rw-------"))]))]
          (swap! state update :paths conj path)
          (try
            (with-open [out (FileOutputStream. (.toFile path))] (copy! (bounded-output out size)))
            (check-interruption!)
            (when-not (= size (Files/size path)) (refuse! :size-mismatch))
            (with-open [in (FileInputStream. (.toFile path))]
              (when-not (= content (blob/blob-id in)) (refuse! :content-mismatch)))
            (check-interruption!)
            (let [buffer (if (zero? size)
                           (.asReadOnlyBuffer (ByteBuffer/allocate 0))
                           (with-open [channel (FileChannel/open path (into-array OpenOption [StandardOpenOption/READ]))]
                             (.map channel FileChannel$MapMode/READ_ONLY 0 size)))
                  entry {:path path :size size :buffer buffer}]
              (swap! state #(-> % (assoc-in [:entries content] entry) (update :bytes + size)))
              {:size size :buffer (.asReadOnlyBuffer ^ByteBuffer buffer)})
            (catch Throwable error
              (try (remove-path! state path)
                   (catch Throwable cleanup-error
                     ;; The retained staging file is not an installed entry and
                     ;; therefore has no capacity charge. Hold this resource
                     ;; rather than admitting more unaccounted failed copies.
                     (swap! state assoc :closed? true)
                     (.addSuppressed error cleanup-error)))
              (throw error))))))))

(defn slice
  "Independent, read-only logical range over an already authorized response."
  [{:keys [size buffer]} offset length]
  (when-not (and (integer? offset) (integer? length) (<= 0 offset size)
                 (<= 0 length (- size offset)))
    (refuse! :invalid-range))
  (let [selected (.asReadOnlyBuffer ^ByteBuffer buffer)]
    (.position selected (int offset))
    (.limit selected (int (+ offset length)))
    (.slice selected)))

(defn close!
  "Call after joining the application's native operations. Queued responses keep
   their own mappings after Unix unlink. Cleanup does not promise immediate
   physical disk/RSS reclamation; failed cleanup stays held for an explicit retry."
  [{:keys [guard state]}]
  (locking guard
    (swap! state assoc :closed? true)
    (doseq [path (:paths @state)] (remove-path! state path))
    (when-let [directory (:directory @state)] (Files/deleteIfExists directory))
    (reset! state {:closed? true :entries {} :bytes 0 :paths #{} :directory nil})
    true))
