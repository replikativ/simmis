(ns is.simm.runtimes.web-url-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.runtimes.web-url :as web-url]))

(deftest websocket-url-test
  (testing "released applications use their exact page origin"
    (is (= "ws://localhost:47295"
           (web-url/websocket-url {:protocol "http:"
                                   :hostname "localhost"
                                   :host "localhost:47295"}
                                  nil)))
    (is (= "ws://localhost:8080"
           (web-url/websocket-url {:protocol "http:"
                                   :hostname "localhost"
                                   :host "localhost:8080"}
                                  "")))
    (is (= "wss://dev.simm.is"
           (web-url/websocket-url {:protocol "https:"
                                   :hostname "dev.simm.is"
                                   :host "dev.simm.is"}
                                  nil))))

  (testing "development override is explicit and independent of page port"
    (is (= "ws://workstation:47295"
           (web-url/websocket-url {:protocol "http:"
                                   :hostname "workstation"
                                   :host "workstation:8087"}
                                  "47295")))
    (is (= "wss://[::1]:47295"
           (web-url/websocket-url {:protocol "https:"
                                   :hostname "[::1]"
                                   :host "[::1]:8080"}
                                  "47295")))))
