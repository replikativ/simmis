(ns is.simm.uis.web.desktop.views.files
  "Files panel — the room drive (doc/archive/file-system-design.md) as a
   browsable tree. Metadata comes from the server-rendered tree
   snapshot (loaded via load-room-drive!); file bytes never travel the
   RPC path: download links hit GET /blobs/<hash>, uploads POST /blobs
   then transact the node by hash. Agents see the same drive at
   /drive in their shells."
  (:require #?(:cljs [is.simm.uis.web.desktop.login :as login])
            [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.effects.track :refer [track]]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [is.simm.uis.web.desktop.chat-remote :as chat-remote])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            [clojure.string :as str]
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.runtimes.web :as web])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [cljs.core.async :refer [go <! put! promise-chan]]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.dom.elements :as el])))

#?(:cljs (defonce ^:private drive-loading (atom #{})))
#?(:cljs (defonce ^:private drive-loaded-cut (atom {})))  ; room-id → last cut-ms loaded

#?(:cljs
   (defn load-room-drive-into-signal!
     "Fire-and-forget load of the room's drive tree into sig/drive-data.
      Root spin via go block — see load-room-details-into-signal!."
     [room-id & {:keys [force? cut-ms]}]
     ;; Re-fetch when the cut changes (files are a REMOTE snapshot — no
     ;; client replica — so a GlobalCut costs an RPC per distinct cut).
     (when (and room-id
                (or force?
                    (not (contains? @drive-loading room-id))
                    (not= (get @drive-loaded-cut (str room-id)) cut-ms)))
       (swap! drive-loading conj room-id)
       (swap! drive-loaded-cut assoc (str room-id) cut-ms)
       (go
         (let [ch (promise-chan)]
           (binding [rtc/*execution-context* runtime]
             (let [s (chat-remote/load-room-drive! web/server-id (str room-id) cut-ms)]
               (s (fn [result] (put! ch {:ok result}))
                  (fn [err] (put! ch {:err err})))))
           (let [{:keys [ok err]} (<! ch)]
             (swap! drive-loading disj room-id)
             (binding [rtc/*execution-context* runtime]
               (if err
                 (rem/report-error! "Could not load this room's files." err)
                 (swap! sig/drive-data assoc (str room-id)
                        (assoc ok :loaded-cut cut-ms))))))))))

(defn- fmt-size [n]
  (cond
    (nil? n) ""
    (< n 1024) (str n " B")
    (< n (* 1024 1024)) (str (int (/ n 1024)) " KB")
    :else (str (.toFixed (/ n (* 1024 1024.0)) 1) " MB")))

#?(:cljs
   (defn- upload-file!
     "POST the browser File to /blobs, then transact the node, then
      refresh the tree."
     [room-id ^js file]
     (-> (.arrayBuffer file)
         (.then (fn [buf]
                  (js/fetch "/blobs"
                            #js {:method "POST"
                                 ;; /blobs is authenticated now — it used to accept
                                 ;; an anonymous write to our disk from anyone.
                                 :headers (login/auth-header-map
                                            #js {"Content-Type"
                                                 (or (not-empty (.-type file))
                                                     "application/octet-stream")})
                                 :body buf})))
         ;; a refused upload (401, 413 …) is not a blob: stop here and say so
         (.then (fn [resp]
                  (if (.-ok resp)
                    (.json resp)
                    (throw (ex-info (str "HTTP " (.-status resp))
                                    {:error (str "upload refused (HTTP " (.-status resp) ")")})))))
         (.then (fn [j]
                  (let [blob-id (.-id j) size (.-size j)]
                    (binding [rtc/*execution-context* runtime]
                      (let [s (chat-remote/put-drive-file!
                               web/server-id (str room-id) ""
                               (.-name file) blob-id
                               (or (not-empty (.-type file)) "application/octet-stream")
                               size)]
                        (s (fn [_] (load-room-drive-into-signal! room-id :force? true))
                           (fn [err] (rem/report-error!
                                      (str "Uploaded " (.-name file)
                                           ", but could not add it to the drive.")
                                      err))))))))
         (.catch (fn [err] (rem/report-error!
                            (str "Could not upload " (.-name file) ".") err))))))

#?(:cljs
   (defn- delete-node! [room-id node-id]
     (binding [rtc/*execution-context* runtime]
       (let [s (chat-remote/delete-drive-node! web/server-id (str room-id) (str node-id))]
         (s (fn [_] (load-room-drive-into-signal! room-id :force? true))
            (fn [err] (rem/report-error! "Could not delete that file." err)))))))


#?(:cljs
   (defn- transcribe-node!
     "Ask the server to transcribe this audio file. It runs there for minutes
      on a long call; the transcript (or why it failed) arrives as a room
      message and a file under transcripts/."
     [room-id node]
     (binding [rtc/*execution-context* runtime]
       (let [s (chat-remote/transcribe-drive-file! web/server-id (str room-id)
                                                   (str (:fs.node/id node)))]
         (s (fn [r]
              (if-let [e (rem/result-failure r)]
                (rem/report-error! (str "Could not transcribe " (:fs.node/name node) ".") e)
                (sig/show-success! (str "Transcribing " (:fs.node/name node)
                                        " — the transcript will appear in the chat"
                                        " and under transcripts/."))))
            (fn [err] (rem/report-error! (str "Could not transcribe " (:fs.node/name node) ".")
                                         err)))))))

(defn- audio-node?
  "Mirrors `transcripts/audio-file?` (server-side) for showing the button."
  [{:fs.node/keys [mime name]}]
  (boolean
   (or (some-> mime (str/starts-with? "audio/"))
       (some-> mime (str/starts-with? "video/"))
       (re-find #"(?i)\.(mp3|m4a|wav|ogg|oga|opus|webm|flac|aac|mp4|mkv|mov)$" (str name)))))

#?(:cljs
   (defn- sha256-hex
     "Browser SHA-256 → lowercase hex — matches the server CAS keying,
      so we can skip uploading blobs the drive already has."
     [buf]
     (-> (js/crypto.subtle.digest "SHA-256" buf)
         (.then (fn [d]
                  (->> (array-seq (js/Uint8Array. d))
                       (map (fn [b] (.padStart (.toString b 16) 2 "0")))
                       (apply str)))))))

#?(:cljs
   (defn- entry-files
     "Recursively collect [{:file File :path \"sub/dir\"} …] from a
      DataTransferItem webkitGetAsEntry tree (promise)."
     [entry path]
     (js/Promise.
      (fn [res rej]
        (cond
          (.-isFile entry)
          (.file entry (fn [f] (res [{:file f :path path}])) rej)

          (.-isDirectory entry)
          (let [reader (.createReader entry)
                acc (atom [])]
            ;; readEntries returns batches; loop until empty.
            (letfn [(step []
                      (.readEntries reader
                        (fn [entries]
                          (if (zero? (.-length entries))
                            (-> (js/Promise.all
                                 (mapv #(entry-files % (str path (when (seq path) "/")
                                                        (.-name entry)))
                                       @acc))
                                (.then (fn [rs] (res (vec (apply concat rs))))))
                            (do (swap! acc into (array-seq entries))
                                (step))))
                        rej))]
              (step)))

          :else (res []))))))

#?(:cljs
   (defn- known-blobs
     "Set of blob hashes already in the loaded tree (dedup on drop)."
     [tree]
     (let [acc (atom #{})]
       (letfn [(walk [nodes]
                 (doseq [n nodes]
                   (when-let [b (:fs.node/blob n)] (swap! acc conj b))
                   (walk (:fs.node/children n))))]
         (walk tree))
       @acc)))

#?(:cljs
   (defn- import-dropped!
     "Import files (from a folder drop) into the drive: hash → skip or
      POST /blobs → node transact. Sequential to keep it simple."
     [room-id items]
     (let [tree (binding [rtc/*execution-context* runtime]
                  (get-in @sig/drive-data [(str room-id) :tree]))
           known (known-blobs tree)
           n (count items)
           done (atom 0)
           ;; Every item bumps `done` whether or not it landed, so the counter
           ;; alone always reaches n/n and the import always looked complete.
           ;; Count the ones that did not, and say so at the end.
           failed (atom [])]
       (binding [rtc/*execution-context* runtime]
         (swap! sig/files-upload-status assoc (str room-id) (str "0/" n)))
       (letfn [(finish! []
                 (binding [rtc/*execution-context* runtime]
                   (swap! sig/files-upload-status dissoc (str room-id)))
                 (when-let [fs (seq @failed)]
                   (rem/report-error!
                    (str "Imported " (- n (count fs)) " of " n " files — "
                         (count fs) " did not land.")
                    (ex-info "import incomplete" {:error (str/join ", " fs)})))
                 (load-room-drive-into-signal! room-id :force? true))
               (bump! []
                 (binding [rtc/*execution-context* runtime]
                   (swap! sig/files-upload-status assoc (str room-id)
                          (str (swap! done inc) "/" n))))
               (put-node! [item blob-id size]
                 (js/Promise.
                  (fn [res _]
                    (binding [rtc/*execution-context* runtime]
                      (let [f (:file item)
                            s (chat-remote/put-drive-file!
                               web/server-id (str room-id) (:path item)
                               (.-name f) blob-id
                               (or (not-empty (.-type f)) "application/octet-stream")
                               size)]
                        (s (fn [_] (res true))
                           (fn [err]
                             (swap! failed conj (.-name f))
                             (js/console.error "[files] node failed:" err)
                             (res false))))))))
               (one [item]
                 (-> (.arrayBuffer (:file item))
                     (.then (fn [buf]
                              (-> (sha256-hex buf)
                                  (.then (fn [hash]
                                           (if (contains? known hash)
                                             (put-node! item hash (.-byteLength buf))
                                             (-> (js/fetch "/blobs"
                                                           #js {:method "POST"
                                                                :headers (login/auth-header-map
                                                                           #js {"Content-Type"
                                                                                (or (not-empty (.-type (:file item)))
                                                                                    "application/octet-stream")})
                                                                :body buf})
                                                 (.then (fn [r]
                                                          (if (.-ok r)
                                                            (.json r)
                                                            (throw (ex-info (str "HTTP " (.-status r)) {})))))
                                                 (.then (fn [j]
                                                          (put-node! item (.-id j) (.-size j)))))))))))
                     (.then (fn [_] (bump!)))
                     (.catch (fn [err]
                               (swap! failed conj (.-name (:file item)))
                               (js/console.error "[files] import item failed:" err)
                               (bump!)))))
               (run [[item & more]]
                 (if item
                   (-> (one item) (.then (fn [_] (run more))))
                   (finish!)))]
         (run (seq items))))))

#?(:cljs
   (defn- on-drop! [room-id ^js e]
     (.preventDefault e)
     (let [entries (->> (array-seq (.. e -dataTransfer -items))
                        (keep #(.webkitGetAsEntry %))
                        vec)]
       (-> (js/Promise.all (mapv #(entry-files % "") entries))
           (.then (fn [rs] (import-dropped! room-id (vec (apply concat rs)))))
           (.catch (fn [err] (rem/report-error!
                              "Could not read what was dropped." err)))))))

(defn- render-node [room-id node depth]
  (let [dir? (= :dir (:fs.node/kind node))
        k (str (:fs.node/id node))]
    (el/div {:class "files-node" :key k}
      (el/div {:class (str "files-row" (when dir? " files-row-dir"))
               :key (str k "-row")
               :style {:padding-left (str (* depth 18) "px")}}
        (vc/icon (if dir? "folder" "file") {:class "files-icon"})
        (if dir?
          (el/span {:class "files-name"} (:fs.node/name node))
          (el/a {:class "files-name files-link"
                 :href (str "/blobs/" (:fs.node/blob node))
                 :download (:fs.node/name node)
                 :target "_blank"}
            (:fs.node/name node)))
        (el/span {:class "files-size"} (fmt-size (:fs.node/size node)))
        #?(:cljs
           (when (and (not dir?) (audio-node? node))
             (el/button {:class "files-transcribe" :title "Transcribe this recording"
                         :on-click (fn [_] (transcribe-node! room-id node))}
               (vc/icon "captions" {:class "files-transcribe-icon"})
               " Transcribe"))
           :clj nil)
        #?(:cljs
           (el/button {:class "files-delete" :title "Delete"
                       :on-click (fn [_] (delete-node! room-id (:fs.node/id node)))}
             (vc/icon "x" {:class "files-delete-icon"}))
           :clj nil))
      (when dir?
        (el/div {:class "files-children"}
          (for [child (:fs.node/children node)]
            (render-node room-id child (inc depth))))))))

#?(:cljs
   (defn files-view
     "The :files tab body — a SELF-TRACKING spin over sig/drive-data
      (same pattern as render-page-editor): the parent column spin
      stays independent of drive state. `data` = {:room-id <uuid-str>}."
     [data]
     (let [room-id (:room-id data)]
       (spin
         (let [gref (iv/get-new (track sig/global-ref))
               cut-ms (some-> (:as-of gref) (.getTime))
               time-travel? (some? cut-ms)
               _ (load-room-drive-into-signal! room-id :cut-ms cut-ms)
               dd-iv (track sig/drive-data)
               dd (iv/get-new dd-iv)
               us-iv (track sig/files-upload-status)
               upload-status (iv/get-new us-iv)
               drive-state (get dd (str room-id))
               ;; only show the tree once it matches the ACTIVE cut (avoid
               ;; flashing the Now tree while the past snapshot loads)
               fresh? (= (:loaded-cut drive-state) cut-ms)
               tree (when fresh? (:tree drive-state))]
           (el/div {:class (str "files-panel" (when time-travel? " files-read-only"))}
             (when time-travel?
               (el/div {:class "files-readonly-note"}
                 "🕰 Viewing a past version — read-only"))
             (el/div {:class "files-toolbar"}
               (el/span {:class "files-drive-name"}
                 (or (get-in drive-state [:drive :drive/name]) "Drive"))
               (el/label {:class "files-upload-btn"}
                 (vc/icon "upload" {:class "files-upload-icon"})
                 " Upload"
                 (el/input {:type "file" :multiple true
                            :style {:display "none"}
                            :on-change
                            (fn [e]
                              (doseq [f (array-seq (.. e -target -files))]
                                (upload-file! room-id f))
                              (set! (.. e -target -value) ""))}))
               (el/button {:class "files-refresh-btn" :title "New folder"
                           :on-click (fn [_]
                                       (when-let [name (js/prompt "Folder name:")]
                                         (when (seq name)
                                           (binding [rtc/*execution-context* runtime]
                                             (let [s (chat-remote/mkdir-drive-path!
                                                      web/server-id (str room-id) name)]
                                               (s (fn [_] (load-room-drive-into-signal!
                                                           room-id :force? true))
                                                  (fn [err] (rem/report-error!
                                                             "Could not create that folder."
                                                             err))))))))}
                 (vc/icon "folder-plus" {:class "files-refresh-icon"}))
               (when-let [st (get upload-status (str room-id))]
                 (el/span {:class "files-upload-status"} (str "Uploading " st "…")))
               (el/button {:class "files-refresh-btn" :title "Refresh"
                           :on-click (fn [_]
                                       (load-room-drive-into-signal! room-id :force? true))}
                 (vc/icon "refresh-cw" {:class "files-refresh-icon"})))
             (el/div {:class "files-tree"
                      :on-dragover (fn [e] (.preventDefault e))
                      :on-drop (fn [e] (on-drop! room-id e))}
               (cond
                 (or (nil? drive-state) (not fresh?))
                 (el/p {:class "files-empty"}
                   (if time-travel? "Loading past snapshot…" "Loading drive…"))

                 (empty? tree)
                 (el/p {:class "files-empty"}
                   "Empty. Upload files (or drop a folder), or ask an agent — the drive is mounted at /drive in their shell.")

                 :else
                 (for [node tree]
                   (render-node room-id node 0)))))))))
   :clj
   (defn files-view [_data]
     (el/div {:class "files-panel"})))
