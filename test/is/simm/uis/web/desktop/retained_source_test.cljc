(ns is.simm.uis.web.desktop.retained-source-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.retained-source :as source]))

(def proposal "11111111-1111-4111-8111-111111111111")
(def component "22222222-2222-4222-8222-222222222222")
(def request-identity {:proposal-id proposal :component-id component :path "/in/knowledge/sources/inbox/A.md"})
(def listing {:proposal proposal :component component :request "33333333-3333-4333-8333-333333333333"
              :selection {:trigger-id proposal :execution-id component :manifest-id proposal}
              :files [{:path (:path request-identity) :source-path "inbox/A.md" :role :trigger :size 2 :content-id component}]})
(defn payload-chunk [offset bytes next-offset]
  {:status :ok :data (merge (select-keys listing [:proposal :component :request :selection])
                           {:path (:path request-identity) :size 2 :content-id component :offset offset
                            :length (count bytes) :encoding "base64"}
                           (when next-offset {:next-offset next-offset}))})

(deftest exact-manifest-source-progress-and-invalid-chunks
  (let [initial (source/accept-list request-identity {:status :listing} {:status :ok :data listing})
        a (source/accept-chunk initial (payload-chunk 0 [65] 1) [65])
        b (source/accept-chunk a (payload-chunk 1 [66] nil) [66])
        ready (source/finish b "AB")]
    (is (= {:kind :chunk :offset 0} (source/step initial)))
    (is (= {:kind :chunk :offset 1} (source/step a)))
    (is (= :received (:status b)))
    (is (= :ready (:status ready)))
    (is (= "AB" (:text ready)))
    (is (not (contains? ready :bytes)))
    (is (nil? (source/step ready)))
    (is (= :unavailable (:status (source/finish b nil))))
    (is (= :unavailable (:status (source/accept-list (assoc request-identity :path "unlisted") {} {:status :ok :data listing}))))
    (is (= :unavailable (:status (source/accept-list request-identity {} {:status :unavailable}))))
    (is (= :unavailable (:status (source/accept-chunk a (payload-chunk 0 [65] 1) [65]))))
    (is (= :unavailable (:status (source/accept-chunk initial (payload-chunk 0 [65] 1) [256]))))))

(deftest source-tab-location-is-independent-and-rejects-retired-owners
  (let [tab {:id :a :type :proposal-source :data request-identity}
        other {:id :b :type :proposal-source :data (assoc request-identity :path "/in/knowledge/sources/inbox/B.md")}
        columns [{:id :left :tabs [tab]} {:id :right :tabs [other]}]]
    (is (= :left (:col-id (source/current-tab columns :a request-identity))))
    (is (= :right (:col-id (source/current-tab [{:id :right :tabs [tab other]}] :a request-identity))))
    (is (nil? (source/current-tab columns :b request-identity)))
    (is (nil? (source/current-tab [{:id :left :tabs [(assoc tab :type :memory)]}] :a request-identity)))
    (is (nil? (source/current-tab [{:id :left :tabs [other]}] :a request-identity)))
    (is (nil? (source/current-tab [{:id :left :tabs [tab tab]}] :a request-identity)))))
