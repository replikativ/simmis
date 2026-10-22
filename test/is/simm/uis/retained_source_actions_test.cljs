(ns is.simm.uis.retained-source-actions-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.render :as render]
            [is.simm.uis.retained-sources-test :as fixture]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.remote :as remote]
            [is.simm.uis.web.desktop.proposals-remote :as rpc]
            [is.simm.uis.web.desktop.views.proposals :as proposals])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(defn with-state [resolved? f]
  (let [ctx (context/fork-context runtime/runtime) calls (atom [])
        epoch (atom {:session-id :fixture :socket :first})]
    (try
      (with-redefs [runtime/runtime ctx
                    connection/snapshot #(deref epoch)
                    rpc/list-proposal-sources! (fn [& args] {:kind :list :args (vec args)})
                    rpc/read-proposal-source! (fn [& args] {:kind :read :args (vec args)})
                    remote/spin! (fn ([effect reply] (swap! calls conj {:rpc (effect) :reply reply}))
                                   ([effect reply _] (swap! calls conj {:rpc (effect) :reply reply})))]
        (binding [ec/*execution-context* ctx]
          (reset! sig/current-user {:id :fixture})
          (reset! sig/proposals-data
                  (if resolved?
                    {:retained-audits {fixture/pid {:state :ready :proposal {:id fixture/pid :status :dismissed}}}}
                    {:proposals [{:id fixture/pid}]}))
          (f calls epoch)))
      (finally (context/close-context! ctx)))))

(defn row []
  (or (first (:proposals @sig/proposals-data))
      (get-in @sig/proposals-data [:retained-audits fixture/pid :proposal])))

(defn state [] (get-in (row) [:retained-sources fixture/scope]))

(defn reply! [calls n result]
  ;; Remote callbacks need not inherit the DOM event's dynamic binding.
  (binding [ec/*execution-context* nil]
    ((:reply (nth @calls n)) result nil)))

(defn listed! [calls]
  (proposals/load-proposal-sources! fixture/pid fixture/scope)
  (reply! calls (dec (count @calls)) {:status :ok :data fixture/listing}))

(deftest hidden-and-replaced-lists-cannot-reappear
  (with-state false
    (fn [calls _]
      (proposals/load-proposal-sources! fixture/pid fixture/scope)
      (proposals/hide-proposal-sources! fixture/pid fixture/scope)
      (reply! calls 0 {:status :ok :data fixture/listing})
      (is (nil? (state)))
      (proposals/load-proposal-sources! fixture/pid fixture/scope)
      (proposals/load-proposal-sources! fixture/pid fixture/scope)
      (reply! calls 1 {:status :ok :data fixture/listing})
      (is (= :loading (:status (state))))
      (reply! calls 2 {:status :ok :data fixture/listing})
      (is (= :ready (:status (state)))))))

(deftest native-byte-decoding-preserves-split-unicode-in-open-and-resolved-cards
  (doseq [resolved? [false true]]
    (with-state resolved?
      (fn [calls _]
        (listed! calls)
        (proposals/load-proposal-source! fixture/pid fixture/scope (:path fixture/entry))
        (reply! calls 1 {:status :ok :data (assoc fixture/first-chunk :base64 "QfA=")})
        (is (= :loading (get-in (state) [:read :status])))
        (is (= 2 (get-in @calls [2 :rpc :args 3 :offset])))
        (reply! calls 2 {:status :ok :data (assoc fixture/second-chunk :base64 "n6aL")})
        (is (= :ready (get-in (state) [:read :status])))
        (is (= "A🦋" (get-in (state) [:read :text])))
        (is (nil? (get-in (state) [:read :bytes])))
        (is (= [:list :read :read] (mapv #(get-in % [:rpc :kind]) @calls)))
        (when resolved? (is (= :dismissed (:status (row)))))))))

(deftest reselect-hide-and-replaced-authentication-stop-held-chunks
  (doseq [replace [:read :hide :socket :session]]
    (with-state false
      (fn [calls epoch]
        (listed! calls)
        (proposals/load-proposal-source! fixture/pid fixture/scope (:path fixture/entry))
        (case replace
          :read (proposals/load-proposal-source! fixture/pid fixture/scope (:path fixture/entry))
          :hide (proposals/hide-proposal-sources! fixture/pid fixture/scope)
          :socket (swap! epoch assoc :socket :replacement)
          :session (reset! sig/current-user {:id :replacement}))
        (let [before @sig/proposals-data count-before (count @calls)]
          (reply! calls 1 {:status :ok :data (assoc fixture/first-chunk :base64 "QfA=")})
          (is (= before @sig/proposals-data))
          (is (= count-before (count @calls))))))))

(deftest malformed-revoked-and-invalid-utf8-never-publish-partial-text
  (doseq [bad [{:status :unavailable}
               {:status :ok :data (assoc fixture/second-chunk :base64 "***")}
               {:status :ok :data (assoc fixture/second-chunk :base64 "n6aL\n")}
               {:status :ok :data (assoc fixture/second-chunk :base64 "YWJj")}
               {:status :ok :data (assoc fixture/second-chunk :request fixture/pid :base64 "n6aL")}]]
    (with-state false
      (fn [calls _]
        (listed! calls)
        (proposals/load-proposal-source! fixture/pid fixture/scope (:path fixture/entry))
        (reply! calls 1 {:status :ok :data (assoc fixture/first-chunk :base64 "QfA=")})
        (reply! calls 2 bad)
        (is (= :unavailable (get-in (state) [:read :status])))
        (is (nil? (get-in (state) [:read :text])))
        (is (nil? (get-in (state) [:read :bytes])))
        (is (= 3 (count @calls)))))))

(deftest frozen-citation-renders-as-inert-readable-text
  (async done
    (let [ctx (context/fork-context runtime/runtime)
          container (.createElement js/document "div") handle (atom nil)
          text "A🦋 proposal\r\n<script>never execute</script>"
          state {:status :ready :listing fixture/listing
                 :read {:status :ready :entry fixture/entry :text text}}
          observer (js/MutationObserver.
                    (fn [_ observer]
                      (when-let [body (.querySelector container "pre")]
                        (.disconnect observer)
                        (try
                          (is (= text (.-textContent body)))
                          (is (false? (.-isContentEditable body)))
                          (is (nil? (.querySelector container "script")))
                          (is (.includes (.-textContent container) "not today's Memory"))
                          (is (.includes (.-textContent container) (:path fixture/entry)))
                          (finally
                            (when-let [stop (:stop! @handle)] (stop))
                            (.remove container)
                            (context/close-context! ctx)
                            (done))))))]
      (set! (.. container -style -display) "none")
      (.appendChild (.-body js/document) container)
      (.observe observer container #js {:childList true :subtree true})
      (binding [ec/*execution-context* ctx]
        (reset! handle
                (render/render-spin! container
                                     (spin (@#'proposals/retained-sources-view fixture/pid fixture/scope state))
                                     (browser/make-dom-discharge js/document)))))))
