(ns is.simm.uis.web.desktop.docx-preview-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [is.simm.uis.web.desktop.docx-preview :as preview]
            ["jszip" :as JSZip]))

(def room "11111111-1111-4111-8111-111111111111")
(def target "22222222-2222-4222-8222-222222222222")
(defn- url [revision]
  (str "/blobs/workspaces/" room "?path=Document.docx&content=" revision "&target=" target))
(def first-id "33333333-3333-4333-8333-333333333333")
(def second-id "44444444-4444-4444-8444-444444444444")
(def third-id "55555555-5555-4555-8555-555555555555")

(defn- wait-for [predicate]
  (js/Promise.
   (fn [resolve reject]
     (let [deadline (+ (.now js/Date) 10000)]
       (letfn [(check []
                 (cond (predicate) (resolve true)
                       (> (.now js/Date) deadline) (reject (js/Error. "DOCX viewer condition timed out"))
                       :else (js/setTimeout check 10)))]
         (check))))))

(defn- document-bytes []
  (let [zip (JSZip.)]
    (.file zip "[Content_Types].xml" "<Types xmlns='http://schemas.openxmlformats.org/package/2006/content-types'><Default Extension='rels' ContentType='application/vnd.openxmlformats-package.relationships+xml'/><Default Extension='xml' ContentType='application/xml'/><Override PartName='/word/document.xml' ContentType='application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml'/></Types>")
    (.file zip "_rels/.rels" "<Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'><Relationship Id='main' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument' Target='word/document.xml'/></Relationships>")
    (.file zip "word/document.xml" "<w:document xmlns:w='http://schemas.openxmlformats.org/wordprocessingml/2006/main' xmlns:r='http://schemas.openxmlformats.org/officeDocument/2006/relationships'><w:body><w:p><w:r><w:t>Document preview test</w:t></w:r></w:p><w:altChunk r:id='html'/></w:body></w:document>")
    (.file zip "word/_rels/document.xml.rels" "<Relationships xmlns='http://schemas.openxmlformats.org/package/2006/relationships'><Relationship Id='html' Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/aFChunk' Target='embedded.html'/></Relationships>")
    (.file zip "word/embedded.html" "<script>parent.parent.postMessage({type:'docx-test-attack'},'*')</script>")
    (.generateAsync zip #js {:type "arraybuffer"})))

(defn- response [bytes]
  (js/Response. (.slice bytes 0) #js {:status 200 :headers #js {"Content-Type" preview/content-type "Content-Length" (str (.-byteLength bytes))}}))

(deftest only-owned-bounded-docx-references-are-accepted
  (is (some? (preview/selection (url first-id) 123 "owner")))
  (doseq [[link size owner] [[(str "https://external.invalid" (url first-id)) 123 "owner"]
                             [(str (url first-id) "&content=" second-id) 123 "owner"]
                             [(str (url first-id) "#fragment") 123 "owner"]
                             [(url first-id) (inc preview/max-bytes) "owner"]
                             [(url first-id) 123 ""]]]
    (is (nil? (preview/selection link size owner)))))

(deftest isolated-render-refresh-authority-loss-and-detach
  (async done
    (let [original js/window.fetch el (.createElement js/document "simmis-docx-preview")
          bytes (atom nil) current (atom nil) release (atom nil) attacks (atom 0)
          receive (fn [event] (when (= "docx-test-attack" (some-> event .-data .-type)) (swap! attacks inc)))]
      (.addEventListener js/window "message" receive)
      (-> (document-bytes)
          (.then (fn [data]
                   (reset! bytes data)
                   (set! (.-fetch js/window)
                         (fn [requested options]
                           (let [s (str requested)]
                             (cond
                               (not (.includes s "/blobs/workspaces/")) (original requested options)
                               (.includes s third-id) (js/Promise.resolve (js/Response. "changed" #js {:status 409}))
                               (.includes s second-id) (js/Promise. (fn [resolve _reject] (reset! release #(resolve (response data)))))
                               :else (js/Promise.resolve (response data))))))
                   (.setAttribute el "data-owner" "owner")
                   (.setAttribute el "data-size" (str (.-byteLength data)))
                   (.setAttribute el "data-url" (url first-id))
                   (.appendChild (.-body js/document) el)
                   (wait-for #(some-> el .-simmisDocx .-currentFrame))))
          (.then (fn [_]
                   (let [frame (.. el -simmisDocx -currentFrame)]
                     (reset! current frame)
                     (is (= "allow-scripts" (.getAttribute frame "sandbox"))))
                   (is (thrown? js/Error (.. @current -contentWindow -document)) "renderer cannot share the app origin")
                   (is (zero? @attacks) "embedded HTML was not executed")
                   (.setAttribute el "data-url" (url second-id))
                   (wait-for #(some? @release))))
          (.then (fn [_]
                   (is (.-isConnected @current) "previous revision remains only during same-scope refresh")
                   (let [pending (.. el -simmisDocx -pending)]
                     (.dispatchEvent js/window
                                     (js/MessageEvent. "message" #js {:origin "null" :source js/window
                                                                      :data #js {:type "simmis:docx-rendered" :id (.-id pending)}}))
                     (is (identical? @current (.. el -simmisDocx -currentFrame)) "forged source cannot complete a render"))
                   (@release)
                   (wait-for #(and (some-> el .-simmisDocx .-currentFrame)
                                   (not (identical? @current (.. el -simmisDocx -currentFrame)))))))
          (.then (fn [_]
                   (is (false? (.-isConnected @current)))
                   (.setAttribute el "data-url" (url third-id))
                   (wait-for #(and (nil? (.. el -simmisDocx -pending))
                                   (.includes (.-textContent el) "Preview unavailable")))))
          (.then (fn [_]
                   (is (nil? (.. el -simmisDocx -currentFrame)) "authorization/target failure removes old preview")
                   (is (zero? (.-length (.querySelectorAll el "iframe"))))
                   (.setAttribute el "data-url" (url first-id))
                   (wait-for #(some-> el .-simmisDocx .-currentFrame))))
          (.then (fn [_]
                   (reset! current (.. el -simmisDocx -currentFrame))
                   (.remove el)
                   (is (nil? (.-simmisDocx el)))
                   (is (false? (.-isConnected @current)))
                   (is (zero? (.-childElementCount el)))))
          (.catch (fn [error] (is false (str error))))
          (.finally (fn []
                      (.remove el)
                      (.removeEventListener js/window "message" receive)
                      (set! (.-fetch js/window) original)
                      (done)))))))
