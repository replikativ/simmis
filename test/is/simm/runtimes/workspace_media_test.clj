(ns is.simm.runtimes.workspace-media-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.capture :as capture]
            [is.simm.documents.capture-test :as capture-data]
            [is.simm.documents.capture-views :as capture-views]
            [is.simm.documents.capture-views-test :as archive-data]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.auth-config :as auth-config]
            [is.simm.runtimes.http-auth :as auth]
            [is.simm.runtimes.media-cache :as cache]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.runtimes.workspace-media :as media])
  (:import [java.nio ByteBuffer]))

(defn- body-text [response]
  (let [^ByteBuffer buffer (:body response) value (byte-array (.remaining buffer))]
    (.get buffer value) (String. value "UTF-8")))

(defn- with-fixture [f]
  (@#'fixture/with-fixture
   true
   (fn [{:keys [owned host w] :as data}]
     (let [owner (lifetime/create) resource (cache/create)
           value (.getBytes "0123456789" "UTF-8") id (blob/blob-id value)
           real-reader auth/blob-reader-party-id]
       (repo/write! owned "captures/frame.png" value)
       (lifetime/open! owner)
       (try
         (with-redefs-fn {#'application/installation (atom {:host (assoc host :media-cache resource) :owner owner})
                         #'auth/blob-reader-party-id :test-party}
           #(let [binding (reads/resolve-host-read! host
                                                  {:principal/kind :human :principal/id (str "party/" (:member w))}
                                                  (str (:room w)))
                  request {:request-method :get :headers {} :test-party (:member w)
                           :path-params {:room (str (:room w))}
                           :query-params {"path" "captures/frame.png" "content" (str id)
                                          "target" (str (:target-stamp binding))}}]
              (f (assoc data :request request :resource resource :owner owner :id id :real-reader real-reader))))
         (finally (lifetime/fence! owner) (lifetime/join! owner 3000) (cache/close! resource)))))))

(deftest head-and-unsatisfiable-range-do-not-load-payloads
  (with-fixture
   (fn [{:keys [request resource]}]
     (with-redefs [content/copy-by-id! (fn [& _] (throw (AssertionError. "Unexpected payload read")))]
       (let [head (media/handle (assoc request :request-method :head))
             invalid (media/handle (assoc-in request [:headers "range"] "bytes=10-"))]
         (is (= 200 (:status head)))
         (is (= "10" (get-in head [:headers "Content-Length"])))
         (is (nil? (:body head)))
         (is (= 416 (:status invalid)))
         (is (= "bytes */10" (get-in invalid [:headers "Content-Range"])))
         (is (nil? (:body invalid)))
         (is (nil? (:directory @(:state resource)))))))))

(deftest archive-recording-link-cannot-follow-replacement-bytes-after-projection
  (with-fixture
   (fn [{:keys [owned request]}]
     (let [seed (capture/observation-note capture-data/session capture-data/segment [])
           path (:path (first (:assets seed)))
           _ (repo/write! owned path (.getBytes "original video fixture" "UTF-8"))
           entry (files/entry @owned path)
           note (capture/observation-note capture-data/session
                                          (merge capture-data/segment (select-keys entry [:content :size])) [])
           _ (repo/write! owned (:path note) (markdown/source-bytes (:text note)))
           result (capture-views/prepare @owned (archive-data/descriptor capture-data/session))
           link (get-in result [:rows 0 :cells 3])
           request (-> request (assoc :request-method :head)
                       (assoc-in [:query-params "path"] (:path link))
                       (assoc-in [:query-params "content"] (str (:content link))))]
       (is (= :media (:kind link)))
       (is (= 200 (:status (media/handle request))))
       (repo/write! owned path (.getBytes "replacement video fixture" "UTF-8"))
       (is (= 409 (:status (media/handle request))))))))

(deftest exact-bytes-ranges-and-cache-hits-still-require-current-authority
  (with-fixture
   (fn [{:keys [request conn w resource]}]
     (let [whole (media/handle request)
           partial (media/handle (assoc-in request [:headers "range"] "bytes=2-4"))]
       (is (= 200 (:status whole)))
       (is (= "0123456789" (body-text whole)))
       (is (= 206 (:status partial)))
       (is (= "234" (body-text partial)))
       (is (= "bytes 2-4/10" (get-in partial [:headers "Content-Range"])))
       (is (= "private, no-store" (get-in partial [:headers "Cache-Control"])))
       (is (= 1 (count (:entries @(:state resource)))))
       (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
       (is (= 409 (:status (media/handle request))))))))

(deftest stale-identities-and-other-rooms-cannot-reuse-cached-bytes
  (with-fixture
   (fn [{:keys [request resource w owned owner]}]
     (is (= 200 (:status (media/handle request))))
     (doseq [bad [(assoc request :test-party (:non-member w))
                   (assoc-in request [:path-params :room] (str (:other-room w)))
                   (assoc-in request [:query-params "target"] (str (random-uuid)))
                   (assoc-in request [:query-params "content"] (str (random-uuid)))]]
       (is (= 409 (:status (media/handle bad)))))
     (is (= 401 (:status (media/handle (dissoc request :test-party)))))
     (repo/write! owned "captures/frame.png" (.getBytes "replacement" "UTF-8"))
     (is (= 409 (:status (media/handle request))))
     (lifetime/fence! owner)
     (is (= 409 (:status (media/handle request))))
     (is (= 1 (count (:entries @(:state resource))))))))

(deftest revocation-during-copy-refuses-the-prepared-response
  (with-fixture
   (fn [{:keys [request conn w]}]
     (let [copy! content/copy-by-id!]
       (with-redefs [content/copy-by-id!
                     (fn [& args]
                       (let [result (apply copy! args)]
                         (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                         result))]
         (is (= 409 (:status (media/handle request)))))))))

(deftest real-blob-cookie-and-route-middleware-retain-room-authority
  (with-fixture
   (fn [{:keys [request w real-reader]}]
     (with-redefs [auth/blob-reader-party-id real-reader]
       (let [handler ((:wrap auth/auth-middleware) media/handle)
             request (-> request (dissoc :test-party)
                         (assoc-in [:reitit.core/match :data :auth] :blob-read))
             cookie (fn [party]
                      (str auth-config/blob-cookie-name "=" (:token (#'auth-config/mint-blob-token party))))]
         (is (= 401 (:status (handler request))))
         (is (= 401 (:status (handler (assoc-in request [:headers "cookie"] "simmis_blob=invalid")))))
         (is (= 200 (:status (handler (assoc-in request [:headers "cookie"] (cookie (:member w)))))))
         (is (= 409 (:status (handler (assoc-in request [:headers "cookie"] (cookie (:non-member w)))))))
         (is (= 200 (:status (handler (-> request (assoc :request-method :head)
                                          (assoc-in [:headers "cookie"] (cookie (:owner w)))))))))))))

(deftest invalidated-room-lease-refuses-the-completed-copy
  (with-fixture
   (fn [{:keys [request room]}]
     (let [copy! content/copy-by-id!]
       (with-redefs [content/copy-by-id!
                     (fn [& args]
                       (let [result (apply copy! args)]
                         (doseq [lease (vals (get @(:meta room) preparation/leases-key))
                                 :when (= :host-operation (:kind lease))]
                           (reset! (:state lease) :cancelled))
                         result))]
         (is (= 409 (:status (media/handle request)))))))))
