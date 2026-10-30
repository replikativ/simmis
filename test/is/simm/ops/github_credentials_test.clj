(ns is.simm.ops.github-credentials-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.git.http :as http]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-credentials :as credentials]
            [is.simm.model.github-integrations-test :as fixtures]
            [is.simm.model.mail-credentials :as envelope]
            [is.simm.ops.github-credentials :as ops]
            [is.simm.ops.github-host :as host]))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn credentials/schema)
        (f conn)
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest credential-rotation-and-revocation-are-cas-and-human-connection-specific
  (fixture
   (fn [conn]
     (let [by (random-uuid) connection (random-uuid) revision (random-uuid)
           encrypted {:nonce "nonce" :ciphertext "ciphertext"}]
       (d/transact conn (credentials/replace-tx @conn by connection nil revision encrypted))
       (let [row (credentials/current-in @conn by connection) bound (credentials/binding-of row)]
         (is (= bound (credentials/current! @conn bound)))
         (is (nil? (credentials/current-in @conn (random-uuid) connection)))
         (is (nil? (credentials/current-in @conn by (random-uuid))))
         (is (thrown? clojure.lang.ExceptionInfo
                      (credentials/replace-tx @conn by connection nil (random-uuid) encrypted)))
         (d/transact conn (credentials/replace-tx @conn by connection row (random-uuid) encrypted))
         (is (thrown? clojure.lang.ExceptionInfo (credentials/current! @conn bound)))
         (is (thrown? clojure.lang.ExceptionInfo (credentials/revoke-tx @conn by connection row (random-uuid))))
         (let [current (credentials/current-in @conn by connection)]
           (d/transact conn (credentials/revoke-tx @conn by connection current (random-uuid))))
         (let [revoked (credentials/current-in @conn by connection)]
           (is (= :revoked (:github.credential/state revoked)))
           (is (nil? (:github.credential/ciphertext revoked)))
           (is (nil? (:github.credential/nonce revoked)))
           (is (nil? (credentials/binding-of revoked)))))))))

(deftest authenticated-transport-refuses-other-destinations-and-stale-grants
  (fixture
   (fn [conn]
     (let [connection (#'fixtures/connection) connection-id (connections/connection-id connection)
           by (random-uuid) context {:conn conn :resolved {:party by}}
           secret {:version 1 :kind :github :by by :connection-id connection-id :token (apply str (repeat 24 "x"))}
           calls (atom 0)]
       (d/transact conn (credentials/replace-tx @conn by connection-id nil (random-uuid)
                                                {:nonce "nonce" :ciphertext "ciphertext"}))
       (with-redefs [host/connection! (fn [supplied] (is (identical? context supplied)) {:connection connection})
                     host/current! (fn [supplied] (is (identical? context supplied)))
                     envelope/decrypt (fn [id nonce ciphertext]
                                        (is (= (credentials/id by connection-id) id))
                                        (is (= ["nonce" "ciphertext"] [nonce ciphertext])) secret)
                     http/send-http (fn [request]
                                      (swap! calls inc)
                                      (is (.startsWith (get-in request [:headers "Authorization"]) "Basic "))
                                      {:status 200})]
         (is (= #{:configured? :revision :connection-id :target-stamp :repository :ref} (set (keys (ops/status! context)))))
         (ops/with-transport!
           context
           (fn [bound]
             (let [send (:git-send bound)]
               (is (= 200 (:status (send {:method :post :url "https://github.com/owner/repository.git/git-receive-pack"}))))
               (doseq [request [{:method :post :url "https://github.com/other/repository.git/git-receive-pack"}
                                {:method :post :url "https://github.com/owner/repository.git.evil/git-receive-pack"}
                                {:method :post :url "http://github.com/owner/repository.git/git-receive-pack"}
                                {:method :get :url "https://github.com/owner/repository.git"}]]
                 (is (thrown? clojure.lang.ExceptionInfo (send request))))
               (let [row (credentials/current-in @conn by connection-id)]
                 (d/transact conn (credentials/revoke-tx @conn by connection-id row (random-uuid))))
               (is (thrown? clojure.lang.ExceptionInfo
                            (send {:method :post :url "https://github.com/owner/repository.git/git-receive-pack"})))
               (is (= 1 @calls)))))
         (is (thrown? clojure.lang.ExceptionInfo (ops/with-transport! context identity))))))))

(deftest provider-errors-do-not-expose-secrets-and-interruption-remains-cancellation
  (fixture
   (fn [conn]
     (let [connection (#'fixtures/connection) connection-id (connections/connection-id connection)
           by (random-uuid) context {:conn conn :resolved {:party by}}
           token (apply str (repeat 24 "x"))]
       (d/transact conn (credentials/replace-tx @conn by connection-id nil (random-uuid)
                                                {:nonce "nonce" :ciphertext "ciphertext"}))
       (with-redefs [host/connection! (fn [_] {:connection connection}) host/current! (fn [_])
                     envelope/decrypt (fn [& _] {:version 1 :kind :github :by by :connection-id connection-id :token token})]
         (doseq [error [(ex-info token {:Authorization token}) (InterruptedException. token)]]
           (try
             (with-redefs [http/send-http (fn [& _] (throw error))]
               (ops/with-transport!
                 context
                 (fn [bound]
                   (try
                     ((:git-send bound) {:method :post :url "https://github.com/owner/repository.git/git-receive-pack"})
                     (is false "Provider exception must be sanitized")
                     (catch clojure.lang.ExceptionInfo failure
                       (let [interrupted? (.isInterrupted (Thread/currentThread))]
                        ;; Test reporting itself may block on nREPL output.
                         (is (= (instance? InterruptedException error) interrupted?)))
                       (is (= :request-failed (:reason (ex-data failure))))
                       (is (nil? (ex-cause failure)))
                       (is (not (.contains (str failure) token))))))))
             (finally (Thread/interrupted)))))))))

(deftest saving-requires-the-inspected-connection-and-revision-before-encrypting
  (fixture
   (fn [conn]
     (let [connection (#'fixtures/connection) connection-id (connections/connection-id connection)
           by (random-uuid) context {:conn conn :resolved {:party by}} revision (random-uuid)
           token (apply str (repeat 24 "x"))]
       (d/transact conn (credentials/replace-tx @conn by connection-id nil revision {:nonce "nonce" :ciphertext "ciphertext"}))
       (with-redefs [host/connection! (fn [_] {:connection connection}) host/current! (fn [_])
                     envelope/encrypt (fn [& _] (throw (AssertionError. "A stale settings form reached encryption")))]
         (is (thrown? clojure.lang.ExceptionInfo (ops/save! context (random-uuid) revision token)))
         (is (thrown? clojure.lang.ExceptionInfo (ops/save! context connection-id nil token)))
         (is (thrown? clojure.lang.ExceptionInfo (ops/save! context connection-id (random-uuid) token)))
         (is (= revision (:github.credential/revision (credentials/current-in @conn by connection-id)))))))))
