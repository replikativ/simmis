(ns is.simm.uis.web.desktop.workspace-editor-test
  (:require [is.simm.uis.web.desktop.workspace-editor :as editor]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(deftest github-import-freezes-canonical-source-and-does-not-retry-on-edit
  (let [draft {:name "Imported source" :source-kind :github
               :repository "https://github.com/Owner/Repo.git"}
        submitted (editor/submit draft (random-uuid) :socket)
        edited (assoc submitted :repository "other/repository")]
    (is (editor/valid? draft))
    (is (= {:kind :github :repository "owner/repo"} (get-in submitted [:submission :source])))
    (is (= (:submission submitted)
           (:submission (editor/submit edited (random-uuid) :reconnected))))
    (is (not (editor/valid? (assoc draft :repository "https://evil.example/owner/repo"))))
    (is (not (editor/valid? (assoc draft :source-kind :arbitrary-provider))))))

(deftest submission-freezes-one-request-and-never-becomes-a-resend
  (let [a (str (random-uuid)) b (str (random-uuid)) id (random-uuid)
        state (editor/submit {:name " Research " :members #{a b}} id :socket)]
    (is (= {:id (str id) :name "Research" :members (vec (sort [a b]))} (:submission state)))
    (is (= :submitting (:phase state)))
    (is (= state (editor/submit state (random-uuid) :socket)))
    (is (= state (editor/submit state (random-uuid) :reconnected)))
    (is (= (:submission state)
           (:submission (editor/observed state (:submission state) {:status :not-observed}))))))

(deftest malformed-or-disconnected-submissions-stay-local
  (doseq [state [{} {:name ""} {:name " "} {:name (apply str (repeat 513 "a"))}
                 {:name "Valid" :members 42} {:name "Valid" :members [nil]}
                 {:name "Valid" :members ["not-a-uuid"]}]]
    (is (not (editor/valid? state)))
    (is (= state (editor/submit state (random-uuid) :socket))))
  (is (= {:name "Valid"} (editor/submit {:name "Valid"} (random-uuid) nil)))
  (is (= {:name "Valid"} (editor/submit {:name "Valid"} nil :socket))))

(deftest original-receipt-survives-late-preparation-and-foreign-acknowledgements
  (let [state (editor/submit {:name "Research"} (random-uuid) :socket)
        request (:submission state)
        result {:status :created :room {:room/id (str (random-uuid)) :room/name "Research" :room/type :project}}
        created (editor/observed state request result)]
    (is (= :created (:phase created)))
    (is (= result (:outcome created)))
    (is (= created (editor/observed created request {:status :preparing})))
    (is (= created (editor/observed created request {:status :not-observed})))
    (is (= state (editor/observed state (assoc request :id (str (random-uuid))) result)))
    (is (= :unconfirmed (:phase (editor/observed state request {:status :created :room {}}))))))
