(ns is.simm.runtimes.instance-test
  "An instance with no settings must resolve EXACTLY what simmis used before
   instances were configurable: the same ports, the same home, the same store
   paths (some are persisted in DBs), the same cookie name."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.runtimes.instance :as instance]))

(def ^:private unset? #(nil? (System/getenv %)))

(deftest port-offset-parsing
  (is (= 0 (instance/parse-port-offset nil)))
  (is (= 0 (instance/parse-port-offset "")))
  (is (= 0 (instance/parse-port-offset "  ")))
  (is (= 1000 (instance/parse-port-offset "1000")))
  (testing "a value that is not an offset refuses to start rather than falling back to the default ports"
    (is (thrown? clojure.lang.ExceptionInfo (instance/parse-port-offset "abc")))
    (is (thrown? clojure.lang.ExceptionInfo (instance/parse-port-offset "-5")))
    (is (thrown? clojure.lang.ExceptionInfo (instance/parse-port-offset "70000")))))

(deftest ports
  (is (= {:nrepl-clj 47888 :nrepl-cljs 9631 :shadow-http 9630 :http 8080 :websocket 47295}
         (instance/ports 0)))
  (is (= {:nrepl-clj 48888 :nrepl-cljs 10631 :shadow-http 10630 :http 9080 :websocket 48295}
         (instance/ports 1000))))

(deftest blob-cookie-name
  (is (= "simmis_blob" (instance/blob-cookie-name 0)))
  (is (= "simmis_blob_1000" (instance/blob-cookie-name 1000))))

(deftest no-settings-resolve-the-established-defaults
  ;; Guarded so a developer who exported a setting does not see a red suite;
  ;; CI sets none of them.
  (when (unset? "SIMMIS_PORT_OFFSET")
    (is (= 0 (instance/port-offset)))
    (is (= instance/default-ports (instance/ports)))
    (is (= "simmis_blob" (instance/blob-cookie-name))))
  (when (and (unset? "SIMMIS_PORT_OFFSET") (unset? "SIMMIS_WS_URL"))
    (is (= "ws://localhost:47295" (instance/websocket-url))))
  (when (unset? "DVERGR_HOME")
    (is (= ".dvergr/jwt-secret" (instance/home-path "jwt-secret")))
    (is (= ".dvergr/room-ft/r1" (instance/home-path "room-ft" "r1"))))
  (when (unset? "SIMMIS_DATA_DIR")
    (is (= "data" (instance/data-dir)))
    (is (= "data/simmis-v2" (instance/data-path "simmis-v2")))
    (is (= "data/simmis-kbs/abc" (instance/data-path "simmis-kbs" "abc")))))
