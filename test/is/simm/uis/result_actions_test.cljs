(ns is.simm.uis.result-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.runtimes.web :as web]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.result-actions :as actions]
            [is.simm.uis.web.desktop.results-remote :as remote]))

(defn with-state [f]
  (let [ctx (context/fork-context runtime/runtime)
        session {:id "result-ui-fixture"}]
    (try
      (with-redefs [runtime/runtime ctx
                    web/current-session (constantly session)
                    web/current-session? #(= session %)]
        (binding [ec/*execution-context* ctx]
          (reset! sig/captured-results {})
          (f ctx (actions/state-key "room" "run" "attempt"))))
      (finally (context/close-context! ctx)))))

(defn deferred [requests]
  (fn [_ _ _ & args]
    (fn [ok error] (swap! requests conj {:ok ok :error error :args args}))))

(defn entry [path size] {:path path :size size :content-id path})
(defn reply [entry offset values next]
  (cond-> {:attempt {:id "attempt"} :path (:path entry) :content-id (:content-id entry)
           :size (:size entry) :offset offset :length (count values) :encoding "base64"
           :media-type "application/octet-stream"
           :base64 (js/btoa (apply str (map #(js/String.fromCharCode %) values)))}
    next (assoc :next-offset next)))

(deftest paging-keeps-prior-data-without-ambient-context-and-through-failure
  (with-state
    (fn [_ k]
      (let [requests (atom []) first-file (entry "/a" 1) next-file (entry "/b" 1)
            prior {:attempt {:id "attempt"} :files [first-file] :offset 0 :next-offset 1 :total 2}]
        (swap! sig/captured-results assoc k {:manifest {:data prior}})
        (with-redefs [remote/load-attempt-files! (deferred requests)]
          (binding [ec/*execution-context* nil] (actions/manifest-page! "room" "run" "attempt" 1))
          (is (= prior (get-in @sig/captured-results [k :manifest :data])))
          ((:error (first @requests)) :denied)
          (is (= prior (get-in @sig/captured-results [k :manifest :data])))
          (is (string? (get-in @sig/captured-results [k :manifest :error])))
          (binding [ec/*execution-context* nil] (actions/manifest-page! "room" "run" "attempt" 1))
          ((:ok (second @requests)) {:attempt {:id "attempt"} :files [next-file] :offset 1 :total 2})
          (is (= [first-file next-file] (get-in @sig/captured-results [k :manifest :data :files]))))))))

(deftest stale-preview-replies-cannot-replace-the-selected-file
  (with-state
    (fn [_ k]
      (let [requests (atom []) a (entry "/a" 1) b (entry "/b" 1)]
        (with-redefs [remote/load-attempt-file! (deferred requests)]
          (actions/preview! "room" "run" "attempt" a)
          (actions/preview! "room" "run" "attempt" b)
          ((:ok (second @requests)) (reply b 0 [66] nil))
          ((:ok (first @requests)) (reply a 0 [65] nil))
          (is (= "/b" (get-in @sig/captured-results [k :preview :path])))
          (is (= "B" (get-in @sig/captured-results [k :preview :text]))))))))

(deftest preview-decodes-descriptor-text-and-keeps-html-inert-as-text
  (is (= "<script>alert(1)</script>" (actions/preview-text (.encode (js/TextEncoder.) "<script>alert(1)</script>") false)))
  (is (= "" (actions/preview-text (js/Uint8Array. 0) false)))
  (is (nil? (actions/preview-text (js/Uint8Array. #js [255 0]) false)))
  (is (nil? (actions/preview-text (js/Uint8Array. #js [65 0 66]) false)))
  (let [bytes (.encode (js/TextEncoder.) "a🦋")]
    (is (= "a🦋" (actions/preview-text bytes false)))
    (is (= "a" (actions/preview-text (.subarray bytes 0 3) true)))
    (is (nil? (actions/preview-text (.subarray bytes 0 3) false)))))

(deftest only-complete-validated-downloads-reach-browser-io
  (with-state
    (fn [_ k]
      (let [requests (atom []) published (atom []) file (entry "/out/binary.bin" 4)]
        (with-redefs [remote/load-attempt-file! (deferred requests)
                      actions/publish-download! (fn [parts path]
                                                  (swap! published conj {:path path :bytes (vec (mapcat array-seq (array-seq parts)))})
                                                  "blob:fixture")]
          (actions/download! "room" "run" "attempt" file)
          ((:ok (first @requests)) (reply file 0 [0 255] 2))
          (is (empty? @published))
          ((:ok (second @requests)) (reply file 2 [128 42] nil))
          (is (= [{:path "/out/binary.bin" :bytes [0 255 128 42]}] @published))
          (is (true? (get-in @sig/captured-results [k :download :complete?])))
          (reset! requests [])
          (actions/download! "room" "run" "attempt" file)
          ((:ok (first @requests)) (reply file 0 [0 255] 2))
          ((:error (second @requests)) :revoked)
          (is (= 1 (count @published)))
          (is (not (get-in @sig/captured-results [k :download :complete?])))
          (reset! requests [])
          (actions/download! "room" "run" "attempt" file)
          ((:ok (first @requests)) (reply file 1 [0 255] 3))
          (is (= 1 (count @published)))
          (is (string? (get-in @sig/captured-results [k :download :error])))
          (reset! requests [])
          (let [empty-file (entry "/out/empty" 0)]
            (actions/download! "room" "run" "attempt" empty-file)
            ((:ok (first @requests)) (reply empty-file 0 [] nil))
            (is (= {:path "/out/empty" :bytes []} (last @published)))))))))

(deftest responses-from-a-prior-session-cannot-publish-a-download
  (with-state
    (fn [_ _]
      (let [requests (atom []) published (atom 0) file (entry "/empty" 0)]
        (with-redefs [remote/load-attempt-file! (deferred requests)
                      actions/publish-download! (fn [& _] (swap! published inc))]
          (actions/download! "room" "run" "attempt" file)
          (with-redefs [web/current-session? (constantly false)]
            ((:ok (first @requests)) (reply file 0 [] nil)))
          (is (zero? @published)))))))
