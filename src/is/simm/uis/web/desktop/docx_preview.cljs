(ns is.simm.uis.web.desktop.docx-preview
  "Disposable browser device. Bytes, frames and network handles never enter
   fork-copied application state; the rendered HTML is never a write source."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.foreign])
  (:require-macros [org.replikativ.spindel.dom.foreign :refer [foreign-node]]))

(def max-bytes (* 8 1024 1024))
(def content-type "application/vnd.openxmlformats-officedocument.wordprocessingml.document")

(defn selection [url size owner]
  (try
    (let [u (js/URL. url (.-origin js/location))
          fields (.-searchParams u)
          path (.get fields "path")
          target (.get fields "target")
          content (.get fields "content")
          uuid? #(boolean (and % (re-matches #"[0-9a-f-]{36}" %)))]
      (when (and (seq owner) (= (.-origin u) (.-origin js/location))
                 (empty? (.-hash u)) (empty? (.-username u)) (empty? (.-password u))
                 (re-matches #"/blobs/workspaces/[0-9a-f-]{36}" (.-pathname u))
                 (= 3 (count (js/Array.from (.keys fields))))
                 (every? #(= 1 (.-length (.getAll fields %))) ["path" "content" "target"])
                 (string? path) (<= 1 (count path) 512) (str/ends-with? (str/lower-case path) ".docx")
                 (uuid? target) (uuid? content) (integer? size) (<= 1 size max-bytes))
        {:url (.-href u) :size size :path path :owner owner
         :scope [owner (.-pathname u) path target]}))
    (catch :default _ nil)))

(defn- remove-frame! [^js frame] (when frame (.remove frame)))

(defn- stop-pending! [^js state]
  (when-let [^js p (.-pending state)]
    (.abort (.-abort p))
    (js/clearTimeout (.-timer p))
    (remove-frame! (.-frame p))
    (set! (.-pending state) nil)))

(defn disconnect! [^js el]
  (when-let [^js state (.-simmisDocx el)]
    (stop-pending! state)
    (remove-frame! (.-currentFrame state))
    (.removeEventListener js/window "message" (.-handler state))
    (set! (.-simmisDocx el) nil))
  (.replaceChildren el))

(defn- current-pending? [^js el ^js state ^js pending]
  (and (.-isConnected el) (identical? state (.-simmisDocx el))
       (identical? pending (.-pending state))))

(defn- fail! [^js el ^js state ^js pending]
  (when (current-pending? el state pending)
    (stop-pending! state)
    ;; A failed refresh must not leave an old authorized revision looking current.
    (remove-frame! (.-currentFrame state))
    (set! (.-currentFrame state) nil)
    (set! (.-currentSelection state) nil)
    (set! (.. state -status -textContent)
          "Preview unavailable. Reopen the current document, use Text and corrections, or download the original.")))

(defn- bounded-body! [^js response]
  (let [declared (js/Number (.get (.-headers response) "content-length"))
        reader (some-> response .-body (.getReader))]
    (when-not (and reader (integer? declared) (<= 1 declared max-bytes))
      (when reader (.cancel reader))
      (throw (js/Error. "Invalid DOCX preview length")))
    (letfn [(read-next [chunks total]
              (-> (.read reader)
                  (.then (fn [^js result]
                           (if (.-done result)
                             (do
                               (when-not (= declared total)
                                 (throw (js/Error. "Incomplete DOCX preview")))
                               (let [out (js/Uint8Array. total)]
                                 (loop [i 0 offset 0]
                                   (when (< i (count chunks))
                                     (let [chunk (nth chunks i)]
                                       (.set out chunk offset)
                                       (recur (inc i) (+ offset (.-byteLength chunk))))))
                                 (.-buffer out)))
                             (let [chunk (.-value result) n (+ total (.-byteLength chunk))]
                               (when (> n declared)
                                 (.cancel reader)
                                 (throw (js/Error. "DOCX preview exceeds the byte limit")))
                               (read-next (conj chunks chunk) n)))))))]
      (read-next [] 0))))

(defn- fetch! [^js el ^js state ^js pending]
  (when-not (.-fetching pending)
    (set! (.-fetching pending) true)
    (-> (js/fetch (:url (.-selection pending))
                  #js {:credentials "same-origin" :mode "same-origin" :redirect "error"
                       :signal (.. pending -abort -signal)})
        (.then (fn [response]
                 (when-not (and (= 200 (.-status response))
                                (str/starts-with? (str/lower-case (or (.get (.-headers response) "content-type") "")) content-type))
                   (throw (js/Error. "DOCX source unavailable")))
                 (bounded-body! response)))
        (.then (fn [bytes]
                 (when (current-pending? el state pending)
                   (when-not (<= 1 (.-byteLength bytes) max-bytes)
                     (throw (js/Error. "DOCX preview exceeds the byte limit")))
                   (.postMessage (.. pending -frame -contentWindow)
                                 #js {:type "simmis:docx-load" :id (.-id pending) :bytes bytes}
                                 "*" #js [bytes]))))
        (.catch (fn [_] (fail! el state pending))))))

(defn- message! [^js el ^js state ^js event]
  (when-let [^js pending (.-pending state)]
    (let [data (.-data event)]
      (when (and (current-pending? el state pending)
                 (identical? (.-source event) (.. pending -frame -contentWindow))
                 (= "null" (.-origin event))
                 data (= 2 (.-length (js/Object.keys data)))
                 (= (.-id pending) (.-id data)))
        (case (.-type data)
          "simmis:docx-ready" (fetch! el state pending)
          "simmis:docx-rendered"
          (when (.-fetching pending)
            (js/clearTimeout (.-timer pending))
            (.abort (.-abort pending))
            (remove-frame! (.-currentFrame state))
            (set! (.-currentFrame state) (.-frame pending))
            (set! (.-currentSelection state) (.-selection pending))
            (.remove (.. pending -frame -classList) "docx-preview-pending")
            (set! (.-pending state) nil)
            (set! (.. state -status -textContent) "Approximate DOCX preview · layout, fields and review markup may differ from Word."))
          "simmis:docx-error" (fail! el state pending)
          nil)))))

(defn- initialize! [^js el owner]
  (let [status (.createElement js/document "div")
        body (.createElement js/document "div")
        state #js {:owner owner :status status :body body}]
    (set! (.-className status) "docx-preview-status")
    (.setAttribute status "role" "status")
    (set! (.-className body) "docx-preview-body")
    (set! (.-handler state) #(message! el state %))
    (set! (.-simmisDocx el) state)
    (.replaceChildren el status body)
    (.addEventListener js/window "message" (.-handler state))
    state))

(defn sync! [^js el]
  (let [owner (.getAttribute el "data-owner")
        selected (selection (.getAttribute el "data-url")
                            (js/Number (.getAttribute el "data-size")) owner)]
    (if-not selected
      (do (disconnect! el)
          (set! (.-textContent el) "Preview requires a current DOCX file within the 8 MiB limit."))
      (let [^js old-state (.-simmisDocx el)
            _ (when (and old-state (not= owner (.-owner old-state))) (disconnect! el))
            ^js state (or (.-simmisDocx el) (initialize! el owner))]
        (when-not (or (= selected (.-currentSelection state))
                      (= selected (some-> (.-pending state) .-selection)))
          (stop-pending! state)
          (when-not (= (:scope selected) (:scope (.-currentSelection state)))
            (remove-frame! (.-currentFrame state))
            (set! (.-currentFrame state) nil)
            (set! (.-currentSelection state) nil))
          (set! (.. state -status -textContent)
                (if (.-currentFrame state) "Updating preview · previous saved revision remains visible…" "Loading document preview…"))
          (let [id (js/crypto.randomUUID)
                frame (.createElement js/document "iframe")
                ^js pending #js {:id id :frame frame :selection selected :abort (js/AbortController.)}]
            (.setAttribute frame "sandbox" "allow-scripts")
            (.setAttribute frame "referrerpolicy" "no-referrer")
            (set! (.-title frame) (str "DOCX · " (last (str/split (:path selected) #"/"))))
            (set! (.-className frame) "docx-preview-frame docx-preview-pending")
            (set! (.-src frame) (str "/docx-preview.html#" id))
            (set! (.-pending state) pending)
            (set! (.-timer pending) (js/setTimeout #(fail! el state pending) 30000))
            (.appendChild (.-body state) frame)))))))

(defn- schedule! [^js el]
  (when-not (.-simmisDocxScheduled el)
    (set! (.-simmisDocxScheduled el) true)
    (js/queueMicrotask (fn []
                         (set! (.-simmisDocxScheduled el) false)
                         (when (.-isConnected el) (sync! el))))))

(when-not (.get js/customElements "simmis-docx-preview")
  (.define js/customElements "simmis-docx-preview"
           (js* "(class extends HTMLElement {
             connectedCallback() { (~{})(this); }
             disconnectedCallback() { (~{})(this); }
             attributeChangedCallback() { if(this.isConnected) (~{})(this); }
             static get observedAttributes() { return ['data-url','data-size','data-owner']; }
           })" schedule! disconnect! schedule!)))

(defn view [id url size]
  (foreign-node {:tag :simmis-docx-preview :key id :class "memory-docx-preview"
                 :data-url url :data-size (str size)
                 :data-owner (pr-str [id (:fork-id ec/*execution-context*)])}))
