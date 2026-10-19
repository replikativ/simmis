(ns is.simm.ops.file-output-review-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.system.db :as registry-db]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.file-output-plan-test :as fixture]
            [is.simm.ops.file-output-review :as review]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.proposal-publication :as publication]
            [is.simm.ops.semantic-diff :as semantic]
            [is.simm.ops.proposals :as proposals]))

(defn- with-review [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}
        _ (d/create-database cfg)
        conn (d/connect cfg)
        party (random-uuid)
        saved (atom nil)]
    (try
      (d/transact conn registry-db/schema)
      (d/transact conn system-db/schema)
      (@#'fixture/with-input
       (fn [source base]
         (reset! saved (plans/plan (assoc base :output (@#'fixture/output! source {"wiki/old.md" "After"}))))))
      (let [{:keys [proposal manifest] :as plan} @saved]
        (with-redefs [system-db/get-conn (constantly conn)
                      publication/publish! (constantly nil)
                      access/authenticated-party-id (constantly party)
                      access/can? (fn [_ subject action target]
                                    (and (= party subject) (= :read action) (= {:room (:room manifest)} target)))]
          (storage/publish! conn plan
                            #(proposals/file-proposal!
                              {:connection conn :id proposal :title "Retained review" :room (:room manifest)
                               :forks [{:scope (:id manifest) :authority-scope (:room manifest)
                                        :branch :retained :system-type :file-output :file-output %}]}))
          (f {:conn conn :party party :proposal proposal :scope (:id manifest) :room (:room manifest)})))
      (finally (d/release conn)))))

(deftest metadata-review-reads-no-bodies-and-one-file-fetch-loads-one-pair
  (with-review
    (fn [{:keys [party scope proposal]}]
      (with-redefs [artifact/get-value (fn [& _] (throw (AssertionError. "Metadata read a body")))]
        (let [summary (review/summary party scope)]
          (is (= [{:path "wiki/old.md" :status :modified :before-bytes 8 :after-bytes 5}]
                 (:files summary)))
          (is (false? (:settlement-supported? summary)))
          (is (= :not-checked (:target-compatibility summary)))))
      (let [get-value artifact/get-value calls (atom 0)]
        (with-redefs [artifact/get-value (fn [& args] (swap! calls inc) (apply get-value args))]
          (is (= {:path "wiki/old.md" :before "Before\r\n" :after "After"}
                 (review/read-file party proposal scope "wiki/old.md"))))
        (is (= 1 @calls))))))

(deftest resolved-proposals-retain-an-authorized-metadata-audit
  (with-review
    (fn [{:keys [conn party proposal scope]}]
      (d/transact conn [{:db/id [:proposal/id proposal] :proposal/status :dismissed}])
      (with-redefs [artifact/get-value (fn [& _] (throw (AssertionError. "Audit read a body")))]
        (let [audit (review/audit party proposal)]
          (is (= (str proposal) (:id audit)))
          (is (= :dismissed (:status audit)))
          (is (= (str scope) (get-in audit [:forks 0 :scope])))
          (is (= "wiki/old.md" (get-in audit [:forks 0 :files 0 :path])))))
      (is (= "After" (:after (review/read-file party proposal scope "wiki/old.md"))))
      (is (nil? (review/audit nil proposal)))
      (is (nil? (review/audit party (random-uuid))))
      (with-redefs [access/can? (constantly false)
                    storage/manifest (fn [& _] (throw (AssertionError. "Denied audit reached manifest")))]
        (is (nil? (review/audit party proposal)))))))

(deftest canonical-review-is-not-a-target-compatibility-check-or-a-paid-summary
  (with-review
    (fn [{:keys [party scope proposal]}]
      (let [p (proposals/get-proposal proposal)]
        (with-redefs [artifact/get-value (fn [& _] (throw (AssertionError. "Review metadata read bodies")))]
          (is (= :review-only (:tier (proposals/proposal-review p))))
          (is (= :not-checked (:target-compatibility (proposals/proposal-review p))))
          (is (= :file-output (:system-type (semantic/semantic-diff scope :retained :file-output :principal party)))))
        (with-redefs [clojure.core/requiring-resolve
                      (fn [& _] (throw (AssertionError. "Retained inspection attempted an unfunded model summary")))]
          (is (nil? (proposals/proposal-summary p []))))))))

(deftest unauthenticated-unreadable-or-wrong-proposal-refuses-before-artifact-access
  (with-review
    (fn [{:keys [party scope proposal]}]
      (with-redefs [storage/manifest (fn [& _] (throw (AssertionError. "Denied reader reached manifest")))]
        (is (thrown? clojure.lang.ExceptionInfo (review/summary nil scope)))
        (with-redefs [access/can? (constantly false)]
          (is (thrown? clojure.lang.ExceptionInfo (review/read-file party proposal scope "wiki/old.md"))))
        (is (thrown? clojure.lang.ExceptionInfo (review/read-file party (random-uuid) scope "wiki/old.md")))
        (doseq [id [nil "" :any]]
          (is (thrown? clojure.lang.ExceptionInfo (review/read-file party id scope "wiki/old.md"))))))))

(deftest canonical-room-binding-cannot-be-replaced-by-the-callers-room-permission
  (with-review
    (fn [{:keys [conn party proposal scope]}]
      (d/transact conn [[:db/add [:proposal/id proposal] :proposal/room (random-uuid)]])
      (with-redefs [access/can? (constantly true)
                    storage/manifest (fn [& _] (throw (AssertionError. "Mismatched room reached manifest")))]
        (is (thrown? clojure.lang.ExceptionInfo (review/read-file party proposal scope "wiki/old.md")))))))

(deftest duplicate-components-or-proposals-refuse-before-body-access
  (doseq [same-proposal? [true false]]
    (with-review
      (fn [{:keys [conn party proposal scope room]}]
        (d/transact conn [{:db/id (if same-proposal? [:proposal/id proposal] -1)
                           :proposal/id (if same-proposal? proposal (random-uuid))
                           :proposal/room room
                           :proposal/forks [{:proposal.fork/scope scope :proposal.fork/branch "duplicate"
                                             :proposal.fork/authority-scope room
                                             :proposal.fork/system-type :file-output
                                             :proposal.fork/file-output [:file-output/id scope]}]}])
        (with-redefs [storage/manifest (fn [& _] (throw (AssertionError. "Ambiguous owner reached manifest")))]
          (is (thrown? clojure.lang.ExceptionInfo (review/read-file party proposal scope "wiki/old.md"))))))))

(deftest captured-server-principal-does-not-depend-on-later-dynamic-bindings
  (with-review
    (fn [{:keys [party proposal scope]}]
      (with-redefs [access/authenticated-party-id (fn [] (throw (AssertionError. "Read dynamic identity after RPC entry")))]
        (is (= "After" (:after (review/read-file party proposal scope "wiki/old.md"))))))))

(deftest room-governance-uses-real-relations-not-the-manifest-as-a-store-scope
  (let [can? access/can?]
    (with-review
      (fn [{:keys [conn party proposal room]}]
        (let [member (random-uuid)]
          (d/transact conn [{:party/id party} {:party/id member}])
          (d/transact conn [{:room/id room :room/owner [:party/id party]
                             :room/parties [[:party/id party] [:party/id member]]}])
          (with-redefs [access/can? can?]
            (let [fork (first (:proposal/forks (proposals/get-proposal proposal)))]
              (is (access/may-merge-proposal-fork? @conn party fork))
              (is (false? (access/may-merge-proposal-fork? @conn member fork)))
              (is (false? (access/may-merge-proposal-fork? @conn party (assoc fork :db/id -99))))
              (d/transact conn [[:db/add [:room/id room] :room/owner [:party/id member]]])
              (is (false? (access/may-merge-proposal-fork? @conn party fork)))
              (is (access/may-merge-proposal-fork? @conn member fork)))))))))
