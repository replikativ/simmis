(ns is.simm.runtimes.workspace-media-wire-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.runtimes.auth-config :as auth-config]
            [is.simm.runtimes.http-auth :as auth]
            [is.simm.runtimes.http-options :as options]
            [is.simm.runtimes.workspace-media :as media]
            [is.simm.runtimes.workspace-media-test :as fixture]
            [org.httpkit.server :as http])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

(defn- header [response name]
  (.orElse (.firstValue (.headers response) name) nil))

(deftest native-http-preserves-head-length-ranges-and-normal-response-framing
  (@#'fixture/with-fixture
   (fn [{:keys [request w real-reader resource]}]
     (with-redefs [auth/blob-reader-party-id real-reader]
       (let [media-handler ((:wrap auth/auth-middleware) media/handle)
             handler (fn [incoming]
                       (case (:uri incoming)
                         "/static" {:status 200 :headers {"Content-Length" "3"}
                                      :body (when-not (= :head (:request-method incoming)) "abc")}
                         "/empty" {:status 204 :body nil}
                         "/error" {:status 503 :body "unavailable"}
                         (media-handler (-> incoming
                                            (assoc :query-params (:query-params request) :path-params (:path-params request))
                                            (assoc-in [:reitit.core/match :data :auth] :blob-read)))))
             server (http/run-server handler (merge (options/server-options "127.0.0.1")
                                                    {:port 0 :legacy-return-value? false}))
             client (-> (HttpClient/newBuilder) (.version HttpClient$Version/HTTP_1_1) (.build))
             base (str "http://127.0.0.1:" (http/server-port server))
             cookie (str auth-config/blob-cookie-name "=" (:token (#'auth-config/mint-blob-token (:member w))))
             send! (fn [method path headers]
                     (let [builder (-> (HttpRequest/newBuilder (URI/create (str base path)))
                                       (.timeout (Duration/ofSeconds 10))
                                       (.method method (HttpRequest$BodyPublishers/noBody)))]
                       (doseq [[name value] headers] (.header builder name value))
                       (.send client (.build builder) (HttpResponse$BodyHandlers/ofByteArray))))]
         (try
           (let [head (send! "HEAD" "/media" {"Cookie" cookie})]
             (is (= 200 (.statusCode head)))
             (is (= "10" (header head "Content-Length")))
             (is (zero? (alength ^bytes (.body head))))
             (is (nil? (:directory @(:state resource))) "HEAD must not materialize payloads"))
           (let [range (send! "GET" "/media" {"Cookie" cookie "Range" "bytes=2-4"})
                 full (send! "GET" "/media" {"Cookie" cookie})
                 invalid (send! "GET" "/media" {"Cookie" cookie "Range" "bytes=10-"})]
             (is (= 206 (.statusCode range)))
             (is (= "3" (header range "Content-Length")))
             (is (= "bytes 2-4/10" (header range "Content-Range")))
             (is (= "234" (String. ^bytes (.body range) "UTF-8")))
             (is (= 200 (.statusCode full)))
             (is (= "10" (header full "Content-Length")))
             (is (= "0123456789" (String. ^bytes (.body full) "UTF-8")))
             (is (= 416 (.statusCode invalid)))
             (is (= "bytes */10" (header invalid "Content-Range")))
             (is (zero? (alength ^bytes (.body invalid)))))
           (is (= 401 (.statusCode (send! "GET" "/media" {}))))
           (doseq [method ["GET" "HEAD"]]
             (let [static (send! method "/static" {})]
               (is (= 200 (.statusCode static)))
               (is (= "3" (header static "Content-Length")))
               (is (= (if (= "HEAD" method) 0 3) (alength ^bytes (.body static))))))
           (is (= 204 (.statusCode (send! "GET" "/empty" {}))))
           (let [error (send! "GET" "/error" {})]
             (is (= 503 (.statusCode error)))
             (is (= "11" (header error "Content-Length")))
             (is (= "unavailable" (String. ^bytes (.body error) "UTF-8"))))
           (finally
             (when-let [stopped (http/server-stop! server {:timeout 1000})]
               (is (= true (deref stopped 5000 ::timeout))))
             (http/server-join server))))))))
