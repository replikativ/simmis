(ns is.simm.uis.proposal-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.remote :as remote]
            [is.simm.uis.web.desktop.views.proposals :as proposals]))

(defn reply [response]
  ;; Match spin!'s multi-arity call convention under CLJS static dispatch.
  (fn
    ([_ callback] (callback response nil))
    ([_ callback _] (callback response nil))))

(deftest successful-decisions-reverify-focused-status-instead-of-reusing-open-cache
  (let [ctx (context/fork-context runtime/runtime)
        retries (atom [])]
    (try
      (with-redefs [runtime/runtime ctx
                    remote/spin! (reply {})
                    proposals/retry-proposal-state! #(swap! retries conj %)]
        (binding [ec/*execution-context* ctx]
          (reset! sig/proposals-data {:proposals [{:id "proposal"}]})
          (proposals/accept! "proposal" false "")
          (proposals/dismiss! "proposal" "")
          (is (= ["proposal" "proposal"] @retries))
          ;; A warning or failed landing remains on the same open card. It
          ;; must not be discarded by the terminal-status refresh path.
          (with-redefs [remote/spin! (reply {:warnings [:conflict]})]
            (proposals/accept! "proposal" false ""))
          (with-redefs [remote/spin! (reply {:failed [{:branch "branch" :error "conflict"}]})]
            (proposals/accept! "proposal" true ""))
          (is (= 2 (count @retries)))
          (is (string? (:action-error (first (:proposals @sig/proposals-data)))))))
      (finally (context/close-context! ctx)))))

(deftest an-open-row-invalidates-earlier-verification-before-external-resolution
  (let [ctx (context/fork-context runtime/runtime)]
    (try
      (with-redefs [runtime/runtime ctx
                    proposals/load-proposals! (constantly nil)]
        (binding [ec/*execution-context* ctx]
          (reset! sig/proposals-data {:proposals []})
          (doseq [terminal [:accepted :dismissed]
                  :let [id (str (random-uuid)) row {:id id}]]
            (with-redefs [remote/spin! (reply {:status :open})]
              (is (= :checking (:status (proposals/proposal-state {:proposals []} id)))))
            ;; The focused inspector, like the chat card, must mark the
            ;; arriving open row as newer than that cached verification.
            (is (= :open (:status (proposals/proposal-state {:proposals [row]} id))))
            ;; A different client resolves it; only its disappearance and a
            ;; fresh server status answer may identify the terminal state.
            (with-redefs [remote/spin! (reply {:status terminal})]
              (is (= terminal (:status (proposals/proposal-state {:proposals []} id)))))
            (proposals/proposal-state {:proposals [row]} id))))
      (finally (context/close-context! ctx)))))
