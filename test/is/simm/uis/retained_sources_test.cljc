(ns is.simm.uis.retained-sources-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.uis.web.desktop.captured-result :as result]
            [is.simm.uis.web.desktop.proposal-card :as card]))

(def pid "11111111-1111-4111-8111-111111111111")
(def scope "22222222-2222-4222-8222-222222222222")
(def other "33333333-3333-4333-8333-333333333333")
(def entry {:path "/in/knowledge/sources/inbox/source.md" :source-path "inbox/source.md"
            :role :trigger :size 5 :content-id other})
(def listing {:proposal pid :component scope :request other
              :selection {:trigger-id pid :execution-id scope :manifest-id other}
              :files [entry]})
(def first-chunk (merge (select-keys listing result/source-identity-keys)
                        {:path (:path entry) :content-id other :size 5
                         :encoding "base64" :offset 0 :length 2 :next-offset 2}))
(def second-chunk (-> first-chunk (assoc :offset 2 :length 3) (dissoc :next-offset)))

(defn listed []
  (-> {:id pid}
      (card/begin-source-list scope :list)
      (card/apply-source-list scope :list {:status :ok :data listing} nil)))

(deftest only-exact-bounded-source-lists-are-accepted
  (is (result/source-list-valid? pid scope listing))
  (is (result/source-list-valid? pid scope (assoc listing :files [])))
  (doseq [bad [(assoc listing :proposal other) (assoc listing :component other)
               (assoc listing :request "not-a-uuid") (assoc listing :selection "not-a-map")
               (assoc listing :files [entry entry])
               (assoc-in listing [:selection :execution-id] "not-a-uuid")
               (assoc-in listing [:files 0 :size] 524289)
               (assoc-in listing [:files 0 :role] :workflow)
               (assoc-in listing [:files 0 :path] "/workflows/private.clj")]]
    (is (not (result/source-list-valid? pid scope bad)))))

(deftest source-chunks-bind-the-full-frozen-selection-and-byte-frontier
  (is (result/source-chunk-valid? listing entry 0 first-chunk 2))
  (is (result/source-chunk-valid? listing entry 2 second-chunk 3))
  (doseq [bad [(assoc first-chunk :proposal other) (assoc first-chunk :component other)
               (assoc first-chunk :request pid) (assoc-in first-chunk [:selection :execution-id] other)
               (assoc first-chunk :content-id pid) (assoc first-chunk :offset 1)
               (assoc first-chunk :length 0 :next-offset 0)
               (assoc first-chunk :length 6) (dissoc first-chunk :next-offset)
               (assoc first-chunk :next-offset 3) (assoc first-chunk :encoding "text")]]
    (is (not (result/source-chunk-valid? listing entry 0 bad (:length bad)))))
  (is (not (result/source-chunk-valid? listing entry 0 first-chunk 3))))

(deftest hidden-replaced-or-failed-lists-cannot-publish-a-late-body
  (let [loading (card/begin-source-read (listed) scope (:path entry) :read)
        response {:status :ok :data first-chunk}
        hidden (card/hide-sources loading scope)
        replaced (card/begin-source-list loading scope :new-list)]
    (is (= hidden (card/apply-source-chunk hidden scope :list :read response [65 240] nil)))
    (is (= hidden (card/apply-source-list hidden scope :list {:status :ok :data listing} nil)))
    (is (= replaced (card/apply-source-chunk replaced scope :list :read response [65 240] nil)))
    (is (= loading (card/apply-source-chunk loading scope :list :older-read response [65 240] nil)))
    (let [failed (card/apply-source-list loading scope :list {:status :unavailable} nil)]
      (is (= :unavailable (get-in failed [:retained-sources scope :status])))
      (is (nil? (get-in failed [:retained-sources scope :read])))
      (is (= failed (card/apply-source-chunk failed scope :list :read response [65 240] nil))))))

(deftest split-utf8-is-retained-as-immutable-bytes-until-the-whole-file-arrives
  (let [loading (card/begin-source-read (listed) scope (:path entry) :read)
        partial (card/apply-source-chunk loading scope :list :read {:status :ok :data first-chunk} [65 240] nil)
        complete (card/apply-source-chunk partial scope :list :read {:status :ok :data second-chunk} [159 166 139] nil)
        ready (card/finish-source-read complete scope :list :read "A🦋")]
    (is (= :loading (get-in partial [:retained-sources scope :read :status])))
    (is (= [65 240] (get-in partial [:retained-sources scope :read :bytes])))
    (is (nil? (get-in partial [:retained-sources scope :read :text])))
    (is (= [65 240 159 166 139] (get-in complete [:retained-sources scope :read :bytes])))
    (is (= :received (get-in complete [:retained-sources scope :read :status])))
    (is (= :ready (get-in ready [:retained-sources scope :read :status])))
    (is (= "A🦋" (get-in ready [:retained-sources scope :read :text])))
    (is (nil? (get-in ready [:retained-sources scope :read :bytes])))
    (is (= complete (card/finish-source-read complete scope :list :older-read "wrong")))
    (is (= :unavailable (get-in (card/finish-source-read complete scope :list :read nil)
                                [:retained-sources scope :read :status])))))

(deftest invalid-chunk-or-revocation-clears-partial-bytes-without-partial-success
  (let [loading (card/begin-source-read (listed) scope (:path entry) :read)
        partial (card/apply-source-chunk loading scope :list :read {:status :ok :data first-chunk} [65 240] nil)]
    (doseq [[response decoded error] [[{:status :unavailable} nil nil]
                                      [{:status :ok :data first-chunk} [65 240] nil]
                                      [{:status :ok :data second-chunk} [-1 166 139] nil]
                                      [nil nil "offline"]]]
      (let [failed (card/apply-source-chunk partial scope :list :read response decoded error)]
        (is (= :unavailable (get-in failed [:retained-sources scope :read :status])))
        (is (nil? (get-in failed [:retained-sources scope :read :bytes])))
        (is (nil? (get-in failed [:retained-sources scope :read :text])))))))
