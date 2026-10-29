(ns is.simm.runtimes.room-apps-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.web.apps :as native]
            [dvergr.discourse :as discourse]
            [dvergr.runtime.preparation :as preparation]
            [dvergr.room.store.memory :as memory]
            [dvergr.system.db :as catalog]
            [muschel.fs :as fs]
            [muschel.fs.geschichte :as gfs]
            [geschichte.repo :as repo]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.authority.world :as world]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.room-apps :as apps]
            [is.simm.runtimes.web :as web]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.runtimes.workflow-schedules :as schedules]
            [is.simm.runtimes.http-auth :as auth]
            [is.simm.runtimes.context :as runtime]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context])
  (:import [java.util.concurrent CountDownLatch]))

(def app-html "<!doctype html><h1>Private app</h1><script>window.appLoaded=true;</script>")

(defn- body-text [response]
  (if (string? (:body response)) (:body response) (slurp (:body response))))

(defn- isolated? [response]
  (and (= "sandbox allow-scripts" (get-in response [:headers "Content-Security-Policy"]))
       (= "no-referrer" (get-in response [:headers "Referrer-Policy"]))))

(defn with-fixture [f]
  (world/with-world [conn w]
    (d/transact conn [{:room/id (:room w) :room/slug "serve-team"}
                      {:room/id (:other-room w) :room/slug "serve-other"}])
    (let [native-room (discourse/make-room {:id (keyword (str "site-" (random-uuid))) :store (memory/make)})
          ctx (:ctx native-room) owner (lifetime/create)
          host {:execution-ctx ctx ::reads/owner {:control-conn conn :execution-ctx ctx}}
          repository (gfs/memory-repository! {:name "room-app-serving"})
          filesystem (gfs/make-root repository) roots (atom [])]
      (fs/mkdir filesystem "/app")
      (fs/mkdir filesystem "/app/js")
      (fs/write-string! filesystem "/app/index.html" app-html false)
      (fs/write-string! filesystem "/app/js/app.js" "window.assetLoaded=true;" false)
      (fs/write-string! filesystem "/outside.txt" "private outside bytes" false)
      (fs/symlink filesystem "/outside.txt" "/app/leak.txt")
      (lifetime/open! owner)
      (try
        (with-redefs-fn {#'application/installation (atom {:host host :owner owner})
                        #'runtime/server-context ctx
                        #'auth/party-id (fn [request] (:test-party request))
                        #'native/app-root (fn [room]
                                            (is (identical? ctx ec/*execution-context*) "Native read uses the original App root")
                                            (swap! roots conj room)
                                            {:fs filesystem :root "/app"})}
          #(f {:conn conn :w w :ctx ctx :host host :owner owner :filesystem filesystem :repository repository :native-room native-room :roots roots
               :request (fn [party path] {:uri path :request-method :get :test-party party})}))
        (finally
          (lifetime/fence! owner)
          (lifetime/join! owner 3000)
          ((:close! repository))
          (discourse/close-room! native-room))))))

(deftest real-read-authority-precedes-native-file-access
  (with-fixture
   (fn [{:keys [w roots request]}]
     (doseq [party [nil (:non-member w) (:admin w) (random-uuid)]]
        (let [response (apps/preview (request party "/apps-preview/serve-team/"))]
         (is (= (if party 403 401) (:status response)))
         (is (isolated? response))
         (is (not (.contains (body-text response) "Private app")))))
     (is (empty? @roots))
     (doseq [party [(:owner w) (:member w)]]
        (let [response (apps/preview (request party "/apps-preview/serve-team/"))]
         (is (= 200 (:status response)))
         (is (isolated? response))
         (is (= app-html (body-text response)))))
      (let [before (count @roots) unknown (apps/preview (request (:owner w) "/apps-preview/not-a-room/"))]
       (is (= 403 (:status unknown)))
       (is (isolated? unknown))
       (is (= before (count @roots)))))))

(deftest actual-resolved-room-cannot-borrow-a-prechecked-slugs-authority
  (with-fixture
   (fn [{:keys [w roots request]}]
     (let [lookups (atom 0)]
       (with-redefs [catalog/room-by-slug (fn [_] (swap! lookups inc) {:room/id (:other-room w) :room/slug "serve-team"})]
          (is (= 403 (:status (apps/preview (request (:owner w) "/apps-preview/serve-team/")))))
         (is (= 1 @lookups))
         (is (empty? @roots)))))))

(deftest native-assets-path-rules-and-redirects-remain-isolated
  (with-fixture
   (fn [{:keys [w request]}]
      (doseq [[path expected] [["/apps-preview/serve-team/js/app.js" 200]
                               ["/apps-preview/serve-team/.git/config" 404]
                               ["/apps-preview/serve-team/js/../../outside.txt" 404]
                               ["/apps-preview/serve-team/leak.txt" 404]
                               ["/apps-preview/serve-team/missing.html" 404]
                               ["/apps-preview/serve-team" 301]]]
        (let [response (apps/preview (request (:member w) path))]
         (is (= expected (:status response)))
         (is (isolated? response))
         (when (= expected 404) (is (not (.contains (body-text response) "private outside bytes"))))))
      (let [response (apps/preview (request (:owner w) "/apps-preview/serve-team"))]
        (is (= "/apps-preview/serve-team/" (get-in response [:headers "Location"]))))
      (is (= 405 (:status (apps/preview (assoc (request (:owner w) "/apps-preview/serve-team/") :request-method :post))))))))

(deftest replaced-control-owner-and-mid-read-revocation-refuse-bytes
  (with-fixture
   (fn [{:keys [conn w roots request]}]
     (with-redefs [system-db/get-conn (constantly (atom @conn))]
        (let [response (apps/preview (request (:owner w) "/apps-preview/serve-team/"))]
         (is (= 503 (:status response)))
         (is (isolated? response))
         (is (empty? @roots))))
     (let [read fs/read-bytes]
       (with-redefs [fs/read-bytes
                     (fn [& args]
                       (let [bytes (apply read args)]
                         (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner [:party/id (:owner w)]]
                                           [:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]])
                         bytes))]
          (let [response (apps/preview (request (:owner w) "/apps-preview/serve-team/"))]
           (is (= 403 (:status response)))
           (is (isolated? response))
           (is (= "room read refused" (body-text response)))))))))

(deftest actual-app-shutdown-joins-a-blocked-native-read
  (with-fixture
   (fn [{:keys [w request]}]
     (let [entered (promise) fenced (promise) gate (CountDownLatch. 1) read fs/read-bytes]
       (with-redefs [fs/read-bytes (fn [& args] (deliver entered true) (.await gate) (apply read args))
                     schedules/stop! (fn [] (deliver fenced true))]
          (let [reader (future (apps/preview (request (:owner w) "/apps-preview/serve-team/")))]
           (is (= true (deref entered 3000 ::timeout)))
           (let [stop (future (application/quiesce!))]
             (try
               (is (= true (deref fenced 3000 ::timeout)))
               (is (not (realized? stop)) "Shutdown cannot release the App while filesystem IO is live")
               (.countDown gate)
               (is (= 200 (:status (deref reader 3000 ::timeout))))
               (is (nil? (deref stop 3000 ::timeout)))
               (finally (.countDown gate) (deref reader 10000 nil) (deref stop 10000 nil))))))))))

(deftest actual-http-route-authenticates-then-checks-the-one-native-resolution
  (with-fixture
   (fn [{:keys [w roots request]}]
     (let [handler (web/wrap-composite-handler (fn [_] {:status 418 :body "websocket fallback"}))
           lookup catalog/room-by-slug lookups (atom 0)]
       (with-redefs [catalog/room-by-slug (fn [slug] (swap! lookups inc) (lookup slug))]
          (let [response (handler (request (:member w) "/apps-preview/serve-team/"))]
           (is (= 200 (:status response)))
           (is (isolated? response))
           (is (= app-html (body-text response)))
           (is (= 1 @lookups))))
       (let [before (count @roots)]
          (is (= 401 (:status (handler (request nil "/apps-preview/serve-team/")))))
         (is (= before (count @roots))))))))

(deftest availability-and-unexpected-failures-are-not-permission-denials
  (with-fixture
   (fn [{:keys [w request]}]
     (doseq [[error status] [[(java.io.IOException. "test file unavailable") 503]
                             [(IllegalStateException. "test unexpected implementation failure") 500]]]
       (with-redefs [native/handle (fn [& _] (throw error))]
          (let [response (apps/preview (request (:owner w) "/apps-preview/serve-team/"))]
           (is (= status (:status response)))
           (is (isolated? response))
           (is (not (.contains (body-text response) (ex-message error))))))))))


(defn with-public-fixture [f]
  (with-fixture
    (fn [{:keys [ctx w repository native-room] :as fixture}]
      (with-redefs [room-agents/registered-room (fn [id] (when (= id (:room w)) native-room))
                    admission/default-binding (fn [_ id _]
                                                (is (= id (:room w)))
                                                {:conn (:conn repository) :registry-system (:repo-sys w)})]
        (f fixture)))))

(deftest public-sites-serve-committed-main-only-without-authentication
  (with-public-fixture
    (fn [{:keys [repository filesystem]}]
      (let [conn (:conn repository) request {:uri "/apps/serve-team/" :request-method :get}]
        (is (= 404 (:status (apps/handle request))) "Uncommitted work is private")
        (repo/stage-all! conn)
        (repo/commit! conn {:message "Publish website"})
        (let [response (apps/handle request)]
          (is (= 200 (:status response)))
          (is (= app-html (body-text response)))
          (is (= "*" (get-in response [:headers "Access-Control-Allow-Origin"])))
          (is (nil? (get-in response [:headers "Access-Control-Allow-Credentials"])))
          (is (isolated? response)))
        (fs/write-string! filesystem "/app/index.html" "PRIVATE NEW DRAFT" false)
        (is (= app-html (body-text (apps/handle request))))
        (repo/create-ref! conn "private-preview")
        (repo/checkout! conn "private-preview" {:force? true})
        (fs/write-string! filesystem "/app/index.html" "PRIVATE BRANCH" false)
        (repo/stage-all! conn) (repo/commit! conn {:message "Private branch"})
        (is (= app-html (body-text (apps/handle request))) "Checkout cannot select public content")
        (doseq [path ["/apps/serve-team/.git/config" "/apps/serve-team/../outside.txt"
                      "/apps/serve-team/%2e%2e/outside.txt" "/apps/serve-team/leak.txt"
                      "/apps/serve-team/outside.txt" "/apps/no-such-room/"]]
          (is (= 404 (:status (apps/handle (assoc request :uri path)))) path))
        (is (= 405 (:status (apps/handle (assoc request :request-method :post)))))
        (is (= "" (:body (apps/handle (assoc request :request-method :head)))))
        (let [handler (web/wrap-composite-handler (constantly {:status 418}))]
          (is (= 200 (:status (handler request))))
          (is (= 401 (:status (handler {:uri "/apps-preview/serve-team/" :request-method :get})))))))))

(deftest public-route-refuses-fork-worlds-and-changed-default-mappings
  (with-public-fixture
    (fn [{:keys [repository ctx w]}]
      (repo/stage-all! (:conn repository))
      (repo/commit! (:conn repository) {:message "Public"})
      (with-redefs [room-agents/registered-room (constantly {:ctx ctx :parent-id :parent})]
        (is (= 404 (:status (apps/handle {:uri "/apps/serve-team/" :request-method :get})))))
      (let [n (atom 0)]
        (with-redefs [admission/default-binding (fn [& _]
                                                  {:conn (:conn repository)
                                                   :registry-system (if (= 1 (swap! n inc)) :before :after)})]
          (is (= 404 (:status (apps/handle {:uri "/apps/serve-team/" :request-method :get})))))))))


(deftest public-body-materialization-owns-its-physical-room
  (with-public-fixture
    (fn [{:keys [repository native-room]}]
      (repo/stage-all! (:conn repository)) (repo/commit! (:conn repository) {:message "Serve"})
      (let [read repo/read-at observed (atom false)]
        (with-redefs [repo/read-at (fn [& args]
                                     (reset! observed (boolean (seq (get @(:meta native-room) preparation/leases-key))))
                                     (apply read args))]
          (is (= 200 (:status (apps/handle {:uri "/apps/serve-team/" :request-method :get})))))
        (is @observed)
        (is (empty? (get @(:meta native-room) preparation/leases-key)))))))
