(ns is.simm.ops.owned-schedule-inputs-test
  "Real managed-artifact/root and original-transaction-ACK proofs only.
   No occurrence dispatcher, provider call, runtime or transport activation."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-inputs :as knowledge]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-schedule-admission-test :as fixture]
            [is.simm.ops.workflow-schedules :as schedules]))

(defn- failure [f]
  (try (f) nil (catch Exception error error)))

(defn- with-input [f]
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host input] :as fixture}]
     (let [row (schedules/create-owned! host (ops/party-principal (:owner w)) (str (:room w)) (input :workflow))
           binding (reads/resolve-host-read! host (ops/party-principal (:owner w)) (str (:room w)))
           db (:db binding)
           policy {:version 1 :trigger-roots ["wiki"] :context-roots [] :exclude-prefixes []}
           captured (knowledge/capture db policy)
           selection (dissoc captured :inputs)
           basis (knowledge/publication-basis db (select-keys (:target binding) [:registry-system :runtime-system]))
           revision (:schedule.activation/revision row)
           input-id (hasch/uuid [:simmis/scheduled-input-v1 revision (:trigger-id selection)])]
       (model/claim-due! conn row {:interval-ms 60000} (:schedule.activation/next-fire row))
       (let [oid (:schedule.activation/active-occurrence (model/activation conn (:schedule.activation/id row)))
             claim (model/admit! conn oid)
             value {:version 2 :occurrence-id oid :input-id input-id :target-revision revision
                    :selection selection :captured captured :publication-basis basis
                    :knowledge-origin {:target (:target binding) :target-stamp (:target-stamp binding)
                                       :selection-id (:execution-id selection)}}]
         (is (= :capture (model/select-input! conn oid claim input-id false)))
         (f (assoc fixture :activation row :oid oid :claim claim :value value)))))))

(deftest live-input-publication-is-rooted-and-read-back-from-the-original-ack
  (with-input
    (fn [{:keys [conn oid claim value]}]
      (let [transact d/transact report (atom nil)]
        (is (nil? (model/retained-owned-input conn (model/occurrence conn oid))))
        (with-redefs [d/transact (fn [& args]
                                   (let [tx (first (second args))]
                                     (is (= (select-keys value [:occurrence-id :target-revision :input-id]) (nth tx 4)))
                                     (is (not (contains? (nth tx 4) :captured))))
                                   (let [r (apply transact args)] (reset! report r) r))]
          (let [ack (model/retain-owned-input! conn oid claim value)]
            (is (= oid (:schedule.occurrence/id ack)))
            (is (= claim (:schedule.occurrence/claim ack)))
            (is (uuid? (:schedule.occurrence/input-ref ack)))
            (is (= (hasch/uuid value) (:schedule.occurrence/input-revision ack)))
            (is (= (:schedule.occurrence/input-ref ack)
                   (:schedule.occurrence/input-ref
                    (d/entity (:db-after @report) [:schedule.occurrence/id oid]))))))
        (let [o (model/occurrence conn oid)]
          (is (= value (model/retained-owned-input conn o)))
          (is (some? (failure #(model/retained-owned-input conn (assoc o :schedule.occurrence/id (random-uuid))))))
          (is (some? (failure #(model/retained-owned-input conn (assoc o :schedule.occurrence/revision (random-uuid))))))
          (is (some? (failure #(model/retain-owned-input! conn oid claim value)))
              "An existing ref cannot become another first-install ACK")
          (is (= o (model/occurrence conn oid))))))))

(deftest frozen-fixture-selection-has-no-live-knowledge-or-publication-authority
  (with-input
    (fn [{:keys [conn oid claim value]}]
      (let [offline (assoc value :captured nil :publication-basis nil :knowledge-origin nil)]
        (model/retain-owned-input! conn oid claim offline)
        (is (= offline (model/retained-owned-input conn (model/occurrence conn oid))))))))

(deftest malformed-envelope-never-reaches-publication
  (with-input
    (fn [{:keys [conn oid claim value]}]
      (let [calls (atom 0)]
        (with-redefs [artifact/publish-value! (fn [& _] (swap! calls inc) (throw (AssertionError. "Invalid envelope published")))]
          (doseq [bad [(assoc value :version 1)
                       (assoc value :unknown true)
                       (dissoc value :knowledge-origin)
                       (assoc value :occurrence-id (random-uuid))
                       (assoc value :input-id (random-uuid))
                       (assoc value :target-revision (random-uuid))
                       (assoc value :captured nil)
                       (assoc value :publication-basis nil)
                       (assoc-in value [:selection :extra] true)
                       (assoc-in value [:captured :extra] true)
                       (assoc-in value [:captured :execution-id] (random-uuid))
                       (assoc-in value [:knowledge-origin :selection-id] (random-uuid))
                       (assoc-in value [:knowledge-origin :target :repository] (random-uuid))
                       (assoc-in value [:captured :inputs "/in/bad"] (str (char 0xD800)))]]
            (is (some? (failure #(model/retain-owned-input! conn oid claim bad)))))
          (is (zero? @calls)))
        (is (nil? (:schedule.occurrence/input-ref (model/occurrence conn oid))))))))

(deftest malformed-or-unowned-claims-never-install-an-input-root
  (with-input
    (fn [{:keys [conn oid claim value]}]
      (let [publications (atom 0)]
        (d/transact conn [[:db/retract [:schedule.occurrence/id oid] :schedule.occurrence/claim claim]])
        (with-redefs [artifact/publish-value! (fn [& _] (swap! publications inc) (throw (AssertionError. "Malformed claim published")))]
          (doseq [bad [nil "not-a-claim"]]
            (is (some? (failure #(model/retain-owned-input! conn oid bad value)))))
          (is (zero? @publications)))
        (is (some? (failure #(model/retain-owned-input! conn oid (random-uuid) value))))
        (is (nil? (:schedule.occurrence/input-ref (model/occurrence conn oid))))))))

(deftest missing-original-transaction-ack-holds-even-when-the-root-was-committed
  (doseq [shape [:missing :before :after]]
    (with-input
      (fn [{:keys [conn oid claim value]}]
        (let [transact d/transact calls (atom 0)]
          (with-redefs [d/transact
                        (fn [& args]
                          (swap! calls inc)
                          (let [report (apply transact args)]
                            (case shape :missing nil :before (dissoc report :db-before) :after (dissoc report :db-after))))]
            (is (some? (failure #(model/retain-owned-input! conn oid claim value)))))
          (is (= 1 @calls))
          (let [stored (model/occurrence conn oid)]
            (is (= :admitting (:schedule.occurrence/state stored)))
            (is (uuid? (:schedule.occurrence/input-ref stored)))
            (is (= value (model/retained-owned-input conn stored)) "Durable observation is not a dispatch ACK")
            (is (some? (failure #(model/retain-owned-input! conn oid claim value))))))))))

(deftest a-no-op-report-cannot-acknowledge-root-publication
  (with-input
    (fn [{:keys [conn oid claim value]}]
      (let [before @conn]
        (with-redefs [d/transact (fn [& _] {:db-before before :db-after before})]
          (is (some? (failure #(model/retain-owned-input! conn oid claim value)))))
        (is (nil? (:schedule.occurrence/input-ref (model/occurrence conn oid))))))))

(deftest actual-encoded-cap-precedes-publication-and-actual-read-cap-precedes-decoding
  (is (= (* 32 1024 1024) model/max-owned-input-bytes))
  (with-input
    (fn [{:keys [conn oid claim value]}]
      (let [calls (atom 0)]
        (is (> (:size (artifact/prepare-value value)) 128))
        (with-redefs [model/max-owned-input-bytes 128
                      artifact/publish-value! (fn [& _] (swap! calls inc) (throw (AssertionError. "Oversize published")))]
          (is (some? (failure #(model/retain-owned-input! conn oid claim value))))
          (is (zero? @calls)))
        (model/retain-owned-input! conn oid claim value)
        (let [o (model/occurrence conn oid) decodes (atom 0)]
          (with-redefs [model/max-owned-input-bytes 128
                        edn/read (fn [& _] (swap! decodes inc) (throw (AssertionError. "Oversize decoded")))]
            (is (some? (failure #(model/retained-owned-input conn o))))
            (is (zero? @decodes))))))))
