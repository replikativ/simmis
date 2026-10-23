(ns is.simm.uis.chat-render-contract-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest chat-rendering-does-not-initialize-writable-projections
  ;; This guards the actual large tab-render entry point, not an independent
  ;; test renderer. Native browser query tests cover the former dependency.
  (let [source (slurp (io/resource "is/simm/uis/web/desktop/views/columns.cljc"))]
    (is (not (str/includes? source "chat-remote/ensure-room!")))
    (is (str/includes? source "(tab-heal/render-chat-tab room-roster)"))))
