(ns is.simm.uis.web.desktop.screen-delivery-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [is.simm.uis.web.desktop.screen-delivery :as delivery]
            [is.simm.uis.web.desktop.screen-sampling :as sampling]))

(deftest synchronous-encoding-failure-does-not-stall-capture
  (async done
    (let [scene [100 100]
          state (atom (sampling/request (sampling/initial :capture) 0 scene))
          errors (atom [])]
      (-> (delivery/submit! (constantly state) (:pending @state)
                            #(throw (js/Error. "canvas unavailable"))
                            #(swap! errors conj (.-message %)))
          (.then (fn [_]
                   (is (= ["canvas unavailable"] @errors))
                   (is (nil? (:pending @state)))
                   (is (nil? (:signature @state)))
                   (swap! state sampling/request 20000 scene)
                   (is (:pending @state))
                   (delivery/submit! (constantly state) (:pending @state)
                                     #(js/Promise.resolve true)
                                     #(is false (str %)))))
          (.then (fn [_]
                   (is (= scene (:signature @state)))
                   (is (nil? (:pending @state)))
                   (is (= @state (sampling/request @state 86400000 scene)))
                   (done)))
          (.catch (fn [error] (is false (str error)) (done)))))))

(deftest a-delayed-upload-cannot-acknowledge-a-replacement-capture
  (async done
    (let [scene [100 100]
          old (sampling/request (sampling/initial :old) 0 scene)
          fresh (sampling/request (sampling/initial :fresh) 0 scene)
          current (atom (atom old))
          release (atom nil)
          uploaded (js/Promise. (fn [resolve _] (reset! release resolve)))
          finished (delivery/submit! #(deref current) (:pending old)
                                      (fn [] uploaded) #(is false (str %)))]
      (reset! current (atom fresh))
      (@release true)
      (-> finished
          (.then (fn [_] (is (= fresh @@current)) (done)))
          (.catch (fn [error] (is false (str error)) (done)))))))
