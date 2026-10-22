(ns is.simm.ops.scheduled-output-test
  "Pure metadata projection checks; no runtime, provider or authority replacement."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.jobs :as jobs]
            [dvergr.room.store :as room-store]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.workflow-schedules :as schedules]))

(defn- fixture [kind]
  (let [party (random-uuid) room (random-uuid) oid (random-uuid) aid (random-uuid)
        revision (random-uuid) rid (requests/request-id party room oid)
        request {:work.request/id rid :work.request/client-id oid :work.request/by party :work.request/room room
                 :work.request/kind kind :work.request/state :submitted :work.request/intent-id (random-uuid)
                 :work.request/publication-basis-id (random-uuid) :work.request/payload-ref (random-uuid)
                 :work.request/job (random-uuid) :work.request/return-pending? false}]
    {:row {:schedule.activation/id aid :schedule.activation/by party :schedule.activation/room room
           :schedule.activation/revision revision}
     :occurrence {:schedule.occurrence/id oid :schedule.occurrence/activation aid
                  :schedule.occurrence/revision revision :schedule.occurrence/request rid}
     :target {:kind kind} :request request
     :job {:id (:work.request/job request) :room :native-room
           :owner (keyword "party" (str party)) :kind (if (= kind :benchmark) :experiment :workflow)}}))

(defn- filed [request kind]
  (let [manifest (random-uuid)
        value (dispositions/make
               (cond-> {:request (:work.request/id request) :intent (:work.request/intent-id request)
                        :job (:work.request/job request) :basis (:work.request/publication-basis-id request)
                        :candidate (random-uuid) :attempt-content (random-uuid) :output (random-uuid) :kind kind}
                 (= :proposed kind) (assoc :manifest manifest :proposal (hasch/uuid [:simmis/knowledge-file-proposal-v1 manifest]))))]
    (into request (map (fn [[k v]] [(keyword "knowledge.disposition" (name k)) v])) value)))

(deftest occurrence-links-require-the-exact-canonical-request-tuple
  (doseq [kind [:workflow :benchmark]]
    (let [{:keys [row occurrence target request]} (fixture kind)
          valid? #(@#'schedules/occurrence-request? row occurrence target %)]
      (is (valid? request))
      (doseq [field [:work.request/id :work.request/client-id :work.request/by :work.request/room]]
        (is (not (valid? (assoc request field (random-uuid))))))
      (is (not (valid? (assoc request :work.request/kind :task))))
      (is (not (valid? (assoc request :work.request/state :admitting))))
      (is (not (@#'schedules/occurrence-request? row (assoc occurrence :schedule.occurrence/request (random-uuid)) target request)))
      (is (not (@#'schedules/occurrence-request? row (assoc occurrence :schedule.occurrence/activation (random-uuid)) target request)))
      (is (not (@#'schedules/occurrence-request? row (assoc occurrence :schedule.occurrence/revision (random-uuid)) target request))))))

(deftest job-links-do-not-borrow-another-rooms-owner-or-run-kind
  (doseq [kind [:workflow :benchmark]]
    (let [{:keys [row target request job]} (fixture kind)
          valid? #(@#'schedules/occurrence-job? :native-room row target request %)]
      (is (valid? job))
      (doseq [[field value] [[:room :foreign-room] [:id (random-uuid)]
                             [:owner (keyword "party" (str (random-uuid)))] [:kind :task]]]
        (is (not (valid? (assoc job field value)))))
      (is (not (@#'schedules/occurrence-job? nil row target request job)))
      (is (not (valid? nil))))))

(deftest filing-links-are-closed-typed-observations-not-approval
  (let [request (:request (fixture :workflow))]
    (is (nil? (@#'schedules/filing-summary request)))
    (is (= {:status :no-op} (@#'schedules/filing-summary (filed request :no-op))))
    (let [row (filed request :proposed) result (@#'schedules/filing-summary row)]
      (is (= #{:status :proposal} (set (keys result))))
      (is (= :proposed (:status result)))
      (is (= (str (:knowledge.disposition/proposal row)) (:proposal result)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (@#'schedules/filing-summary (assoc row :knowledge.disposition/job (random-uuid))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (@#'schedules/filing-summary (assoc request :knowledge.disposition/id (random-uuid)))))))

(defn- summary
  "Run only in an isolated test JVM: native lookup functions are replaced."
  [{:keys [row occurrence target request]} status]
  (with-redefs [d/pull (fn [& _] {:room/slug "native-room"})
                requests/get-request (fn [& _] request)
                model/recent-occurrences (fn [& _] [(assoc occurrence :schedule.occurrence/intended-at (java.util.Date. 0)
                                                                    :schedule.occurrence/coalesced 0)])
                jobs/status status]
    (@#'schedules/schedule-summary (atom :unused) (:schedule.activation/room row)
     (assoc row :schedule.activation/next-fire (java.util.Date. 60000)) target)))

(deftest summary-rejects-foreign-pointers-before-any-job-lookup
  (let [data (fixture :workflow)
        data (assoc-in data [:occurrence :schedule.occurrence/request] (random-uuid))
        result (summary data (fn [& _] (throw (AssertionError. "Foreign pointer reached Job lookup"))))
        occurrence (first (:occurrences result))]
    (is (nil? (:job occurrence)))
    (is (nil? (:status occurrence)))
    (is (nil? (:request occurrence)))
    (is (not (contains? occurrence :filing)))))

(deftest summary-retains-authorized-links-but-not-private-receipt-fields
  (doseq [kind [:workflow :benchmark]]
    (let [data (fixture kind)
          data (cond-> data (= :workflow kind) (update :request filed :proposed))
          job (assoc (:job data) :room (room-store/slug->room-id "native-room") :status :completed :settled? true)
          occurrence (first (:occurrences (summary data (constantly job))))]
      (is (= (str (:id job)) (:job occurrence)))
      (is (= :completed (:status occurrence)))
      (is (true? (:settled? occurrence)))
      (is (= (str (get-in data [:request :work.request/id])) (:request occurrence)))
      (if (= kind :workflow)
        (is (= #{:status :proposal} (set (keys (:filing occurrence)))))
        (is (not (contains? occurrence :filing)))))))

(deftest historical-filing-does-not-depend-on-a-live-job-or-leak-a-foreign-one
  (let [data (update (fixture :workflow) :request filed :no-op)]
    (doseq [job [nil (assoc (:job data) :room :foreign-room :status :running :settled? false)]]
      (let [occurrence (first (:occurrences (summary data (constantly job))))]
        (is (nil? (:job occurrence)))
        (is (nil? (:status occurrence)))
        (is (nil? (:settled? occurrence)))
        (is (= {:status :no-op} (:filing occurrence)))))))
