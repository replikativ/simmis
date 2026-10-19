(ns is.simm.uis.web.desktop.memory-actions
  "Context/session-owned Memory I/O. Late replies cannot replace another
   view's draft, and no browser write is sent to a replicated Datahike store."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.runtimes.web :as web]))

(defn state-key [data]
  [(:id (web/current-session)) (str (:room-id data)) (:col-id data)])

(defn- parent [path] (str/join "/" (butlast (str/split path #"/"))))

(defn controller [data]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        room (str (:room-id data)) key (state-key data)
        current? #(web/current-session? session)
        state #(binding [ec/*execution-context* ctx] (get @sig/memory-editor-state key))
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (current?) (apply swap! sig/memory-editor-state update key f args))))
        change! (fn [path f & args] (apply update! update-in [:drafts path] f args))
        request!
        (fn [slot invoke consume & [after]]
          (let [token (str (random-uuid))
                valid? #(and (:id session) (current?) (editor/response-current? (state) slot token))
                finish! (fn [f] (binding [ec/*execution-context* ctx] (when (valid?) (update! f))))]
            (update! #(-> % (assoc-in [:requests slot] token) (assoc-in [:loading slot] true)
                          (update :errors dissoc slot)))
            (try
              (binding [ec/*execution-context* ctx ec/*spin-id* nil]
                (let [s (invoke)]
                  (s (fn [reply]
                       (if-let [error (rem/result-failure reply)]
                         (finish! #(-> % (assoc-in [:loading slot] false)
                                       (assoc-in [:errors slot] (rem/error-text error))))
                         (try
                           (when (finish! #(-> (consume % reply)
                                               (assoc-in [:loading slot] false)
                                               (cond-> (:permissions reply) (assoc :permissions (:permissions reply)))))
                             ;; Device/I/O effects are outside the state transform.
                             (when after (after reply)))
                           (catch :default e
                             (finish! #(-> % (assoc-in [:loading slot] false)
                                           (assoc-in [:errors slot] (rem/error-text e))))))))
                     (fn [error]
                       (finish! #(-> % (assoc-in [:loading slot] false)
                                     (assoc-in [:errors slot] (rem/error-text error))))))))
              (catch :default error
                (finish! #(-> % (assoc-in [:loading slot] false)
                              (assoc-in [:errors slot] (rem/error-text error))))))))]
    (letfn [(folder! [path]
              (request! [:folder path] #(remote/load-memory-folder! web/server-id room path)
                        #(assoc-in %1 [:folders path] %2)))
            (analyze! [path]
              (let [text (get-in (state) [:drafts path :text])]
                (when (string? text)
                  (request! [:analysis path] #(remote/analyze-memory-text! web/server-id room path text)
                            #(update-in %1 [:drafts path] editor/analyzed text %2)))))
            (open! [path refresh?]
              (update! assoc :selected path)
              (when (or refresh? (nil? (get-in (state) [:drafts path])))
                (let [generation (get-in (state) [:drafts path :generation])]
                  (request! [:file path] #(remote/load-memory-file! web/server-id room path)
                            (fn [s reply]
                              ;; A save/move completed since this read began.
                              (update-in s [:drafts path] editor/loaded-at-generation generation reply))))))
            (save! [path]
              (let [draft (get-in (state) [:drafts path]) sent (:text draft)]
                (when (and (not (:rich-edit draft)) (not (get-in (state) [:loading [:save path]]))
                           (not (get-in (state) [:loading [:move path]])))
                  (request! [:save path]
                            #(remote/save-memory-file! web/server-id room
                                                       {:path path :expected (get-in draft [:source :entry]) :text sent})
                            (fn [s reply] (update-in s [:drafts path] editor/saved-at-generation (:generation draft) sent reply))
                            (fn [_] (folder! (parent path)))))))
            (mode! [path mode]
              (when-not (get-in (state) [:drafts path :rich-edit])
                (change! path assoc :mode mode)
                (when (and (not= mode :source) (nil? (get-in (state) [:drafts path :analysis])))
                  (analyze! path))))
            (create! [path]
              (when (and path (not (str/blank? path)))
                (let [markdown? (re-find #"(?i)\.(md|markdown)$" path)
                      text (if markdown? (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# Untitled\n\n") "")
                      file {:path path :text "" :entry nil :kind (if markdown? :markdown :text)}]
                  (update! assoc :selected path)
                  (change! path #(or % (assoc (editor/draft file) :text text :mode :source))))))
            (download! [path]
              (let [entry (get-in (state) [:drafts path :source :entry])]
                (request! [:download path]
                          #(remote/download-memory-file! web/server-id room path entry)
                          (fn [s reply] (assoc s :download reply)))))
            (upload! [folder file]
              (let [path (str (when (seq folder) (str folder "/")) (.-name file))]
                (if (> (.-size file) (* 8 1024 1024))
                  (update! assoc-in [:errors :upload] "File exceeds the 8 MiB interactive upload limit.")
                  (-> (.arrayBuffer file)
                      (.then (fn [buffer]
                               (when (current?)
                                 (let [bytes (js/Uint8Array. buffer)
                                       chunks (for [from (range 0 (.-length bytes) 16384)]
                                                (.apply (.-fromCharCode js/String) nil
                                                        (.subarray bytes from (min (+ from 16384) (.-length bytes)))))
                                       payload (js/btoa (apply str chunks))]
                                   (binding [ec/*execution-context* ctx]
                                     (request! [:upload path]
                                               #(remote/save-memory-file! web/server-id room
                                                                          {:path path :expected nil :base64 payload})
                                               (fn [s _] s)
                                               (fn [_] (folder! folder) (open! path true))))))))
                      (.catch (fn [e] (update! assoc-in [:errors :upload] (rem/error-text e))))))))
            (office-open! [path part]
              (let [draft (get-in (state) [:drafts path]) generation (:generation draft)
                    expected (get-in draft [:source :entry])]
                (if (or (editor/dirty? draft) (get-in (state) [:loading [:office-save path]]))
                  (update! assoc-in [:errors :office] "Save or discard this XML draft before switching parts.")
                  (request! [:office path]
                            #(remote/load-memory-office-part! web/server-id room path expected part)
                            (fn [s reply]
                              (if (and (= generation (get-in s [:drafts path :generation]))
                                       (not (editor/dirty? (get-in s [:drafts path]))))
                                (assoc-in s [:drafts path :office] (assoc reply :saved-text (:text reply))) s))))))
            (office-save! [path]
              (let [draft (get-in (state) [:drafts path]) office (:office draft) sent (:text office)]
                (when-not (or (get-in (state) [:loading [:office-save path]])
                              (get-in (state) [:loading [:move path]]))
                  (request! [:office-save path]
                            #(remote/save-memory-office-part! web/server-id room
                                                             {:path path :expected (get-in draft [:source :entry])
                                                              :part (:part office) :text sent})
                            (fn [s reply]
                              (if (= (:part office) (get-in s [:drafts path :office :part]))
                                (update-in s [:drafts path]
                                         #(-> % (assoc-in [:source :entry] (:entry reply))
                                              (assoc-in [:office :saved-text] sent)
                                              (assoc :last-commit (:commit reply))
                                              (update :generation inc))) s))
                            (fn [_] (folder! (parent path)))))))]
      {:owner key :state state :change change! :folder folder! :open #(open! % false) :reload #(open! % true)
       :ensure-root (fn []
                      (when (and (:id session) (current?)
                                 (nil? (get-in (state) [:folders ""]))
                                 (not (get-in (state) [:requests [:folder ""]])))
                        (folder! "")))
       :analyze analyze! :save save! :mode mode! :create create! :download download! :upload upload!
       :office-open office-open! :office-save office-save!
       :root (fn [] (update! assoc :current-folder "") (folder! ""))
       :mkdir (fn [path]
                (request! [:mkdir path]
                          #(remote/save-memory-file! web/server-id room
                                                     {:path (str path "/.keep") :expected nil :text ""})
                          (fn [s _] (-> s (assoc :current-folder path) (assoc-in [:expanded path] true)))
                          (fn [_] (folder! (parent path)) (folder! path))))
       :toggle (fn [path]
                 (let [expanded (get-in (state) [:expanded path])]
                   (update! assoc-in [:expanded path] (not expanded))
                   (update! assoc :current-folder path)
                   (when (and (not expanded) (nil? (get-in (state) [:folders path]))) (folder! path))))
       :refresh (fn []
                  (update! assoc :errors {})
                  (doseq [folder (keys (:folders (state)))] (folder! folder))
                  (when-let [path (:selected (state))] (open! path true)))
       :use-incoming (fn [path]
                       (when (js/confirm "Replace this local draft with the newer saved version?")
                         (change! path editor/use-incoming)
                         (update! update :errors dissoc [:save path] [:file path] [:rich path])))
       :start-rich (fn [path slot]
                     (change! path assoc :rich-edit {:slot slot :model (:model slot) :token (str (random-uuid))}))
       :change-rich (fn [path token model selection]
                      (when (= token (get-in (state) [:drafts path :rich-edit :token]))
                        (change! path update :rich-edit assoc :model model :selection selection)))
       :cancel-rich #(change! % dissoc :rich-edit)
       :apply-rich
       (fn [path]
         (let [draft (get-in (state) [:drafts path]) region (:rich-edit draft)
               text (:text draft) token (:token region) slot (:slot region)]
           (request! [:rich path]
                     #(remote/apply-memory-rich-edit! web/server-id room
                                                      {:path path :text text :revision (get-in draft [:analysis :revision])
                                                       :from (:from slot) :to (:to slot) :model (:model region)})
                     (fn [s reply]
                       (if (and (= token (get-in s [:drafts path :rich-edit :token]))
                                (= (:model region) (get-in s [:drafts path :rich-edit :model]))
                                (= text (get-in s [:drafts path :text])))
                         (update-in s [:drafts path]
                                    #(-> % (assoc :text (:text reply) :analysis (:analysis reply)
                                                  :analysis-text (:text reply)) (dissoc :rich-edit)))
                         (assoc-in s [:errors [:rich path]] "The region changed during validation. Your newer edits are retained; apply again."))))))
       :move
       (fn [path target]
         (let [draft (get-in (state) [:drafts path])]
           (if (or (editor/dirty? draft) (:rich-edit draft) (get-in (state) [:loading [:save path]])
                   (get-in (state) [:loading [:office-save path]])
                   (get-in (state) [:loading [:move path]]) (get-in (state) [:drafts target]))
             (update! assoc-in [:errors :move] "Finish pending saves and choose a destination without another open draft before moving.")
             (request! [:move path]
                       #(remote/move-memory-file! web/server-id room
                                                  {:path path :target target :expected (get-in draft [:source :entry])})
                       (fn [s reply]
                         (let [moved (editor/moved (get-in s [:drafts path]) reply)]
                           (if (get-in s [:drafts target])
                             ;; Never overwrite a separate target draft.
                             (assoc-in s [:errors :move] "File moved; both local drafts were retained. Refresh before saving either draft.")
                             (-> s (assoc-in [:drafts target] moved) (update :drafts dissoc path)
                                 (assoc :selected (:path reply))))))
                       (fn [_] (folder! (parent path)) (folder! (parent target)) (open! target true))))))})))
