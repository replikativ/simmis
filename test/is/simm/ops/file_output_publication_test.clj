(ns is.simm.ops.file-output-publication-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.blob :as blob]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.ops.file-output-publication :as publication]
            [is.simm.ops.knowledge-outputs :as outputs])
  (:import [java.util Base64]))

(defn- body [text]
  (let [bytes (markdown/source-bytes text)]
    {:content (blob/blob-id bytes) :bytes (alength bytes) :mode 33188
     :base64 (.encodeToString (Base64/getEncoder) bytes)}))

(defn- fixture []
  (let [old (body "Before\r\n") after (body "After\r\n") new (body "# New\n")
        target {:repository (random-uuid) :store (random-uuid) :backend :memory :branch :db
                :ref "refs/heads/main" :registry-system (random-uuid) :runtime-system "owned-runtime"}
        manifest {:version 1 :request (random-uuid) :room (random-uuid) :attempt (random-uuid)
                  :environment (random-uuid) :workflow (random-uuid) :basis (random-uuid) :output (random-uuid)
                  :target target
                  :entries {"wiki/old.md" {:before (assoc (select-keys old [:content :bytes :mode]) :state :present)
                                           :after (select-keys after [:content :bytes :mode])}
                            "wiki/new.md" {:before {:state :absent} :after (select-keys new [:content :bytes :mode])}}}
        manifest (assoc manifest :id (hasch/uuid [:simmis/knowledge-file-manifest-v1 manifest]))
        proposal (hasch/uuid [:simmis/knowledge-file-proposal-v1 (:id manifest)])
        plan {:manifest manifest :proposal proposal
              :bodies {"wiki/old.md" {:before old :after after} "wiki/new.md" {:before nil :after new}}}
        operation (adoptions/make {:proposal proposal :component (:id manifest) :manifest (:id manifest)
                                   :room (:room manifest) :actor (random-uuid) :filing (apply str (repeat 64 "a"))
                                   :disposition (random-uuid) :target target} (random-uuid) (java.util.Date.))
        commit (random-uuid)
        candidate (merge (publication/identity operation manifest)
                         {:publication/noop? false :publication/commit commit :publication/head commit
                          :publication/created-at (java.util.Date.)
                          :publication/entries
                          (mapv (fn [[path {:keys [after]}]]
                                  {:publication.entry/path (str "/" path) :publication.entry/state :present
                                   :publication.entry/content [:geschichte.content/id (:content after)]
                                   :publication.entry/size (:bytes after) :publication.entry/mode (:mode after)})
                                (sort-by key (:entries manifest)))})
        receipt (-> candidate (assoc :db/id 100)
                    (update :publication/entries
                            #(mapv (fn [entry]
                                     (-> entry (assoc :db/id 101)
                                         (update :publication.entry/content
                                                 (fn [[_ id]] {:db/id 201 :geschichte.content/id id})))) %)))]
    {:operation operation :plan plan :manifest manifest :candidate candidate :receipt receipt}))

(deftest prepare-is-exact-closed-bounded-data-and-never-a-callback
  (let [{:keys [operation plan manifest]} (fixture)
        result (publication/prepare operation plan)]
    (is (= #{:identity :files :options} (set (keys result))))
    (is (= #{:expected-worktree :file-modes} (set (keys (:options result)))))
    (is (= #{"/wiki/old.md" "/wiki/new.md"} (set (keys (:files result)))))
    (is (= {:base64 (get-in plan [:bodies "wiki/old.md" :after :base64])}
           (get-in result [:files "/wiki/old.md"])))
    (is (nil? (get-in result [:options :expected-worktree "/wiki/new.md"])))
    (is (= {:content (get-in manifest [:entries "wiki/old.md" :before :content])
            :size (get-in manifest [:entries "wiki/old.md" :before :bytes]) :mode 33188}
           (get-in result [:options :expected-worktree "/wiki/old.md"])))
    (is (= {"/wiki/old.md" 33188 "/wiki/new.md" 33188} (get-in result [:options :file-modes])))
    (is (= (:actor operation) (get-in result [:identity :publication/actor])))
    (is (= (:attempt manifest) (get-in result [:identity :publication/attempt])))
    (is (= (publication/identity operation manifest) (:identity result)))))

(deftest metadata-and-binding-refuse-before-any-body-decode
  (let [{:keys [operation plan manifest]} (fixture)]
    (with-redefs [outputs/bytes! (fn [& _] (throw (AssertionError. "Decoded before rejecting metadata")))]
      (doseq [bad [(assoc plan :manifest (assoc manifest :id (random-uuid)))
                   (assoc plan :manifest (assoc-in manifest [:entries "wiki/new.md" :after :bytes] 1000000000))
                   (assoc plan :manifest (assoc-in manifest [:target :store] (random-uuid)))]]
        (is (thrown? clojure.lang.ExceptionInfo (publication/prepare operation bad))))
      (let [changed (assoc operation :state :aborted :reason :witnessed-failure)]
        (is (thrown? clojure.lang.ExceptionInfo (publication/prepare changed plan)))))
    (is (thrown? clojure.lang.ExceptionInfo (publication/prepare operation (assoc-in plan [:bodies "wiki/new.md" :after :base64] "bad"))))
    (is (thrown? clojure.lang.ExceptionInfo (publication/prepare operation (assoc plan :callback (fn [] (throw (AssertionError.)))))))))

(deftest operation-binding-must-match-the-exact-manifest
  (let [{:keys [operation manifest]} (fixture)
        remake (fn [changes]
                 (adoptions/make (merge (select-keys operation [:proposal :component :manifest :room :actor :filing :disposition :target]) changes)
                                 (random-uuid) (java.util.Date.)))]
    (doseq [changes [{:proposal (random-uuid)} {:room (random-uuid)}
                     (let [id (random-uuid)] {:component id :manifest id})
                     {:target (assoc (:target operation) :runtime-system "different-runtime")}]]
      (is (thrown? clojure.lang.ExceptionInfo (publication/identity (remake changes) manifest))))
    (is (thrown? clojure.lang.ExceptionInfo (publication/identity (assoc operation :actor (random-uuid)) manifest)))))

(deftest candidate-and-winning-receipt-are-not-interchangeable
  (let [{:keys [operation manifest candidate receipt]} (fixture)]
    (is (= candidate (publication/validate-candidate! operation manifest candidate)))
    (is (= receipt (publication/validate-receipt! operation manifest receipt)))
    (is (thrown? clojure.lang.ExceptionInfo (publication/validate-receipt! operation manifest candidate)))
    (is (thrown? clojure.lang.ExceptionInfo (publication/validate-candidate! operation manifest receipt)))
    ;; No DB/current HEAD/read is required for the original historical receipt.
    (let [reordered (update receipt :publication/entries #(vec (reverse %)))]
      (is (= reordered (publication/validate-receipt! operation manifest reordered))))))

(deftest every-publication-identity-field-and-exact-entry-set-are-required
  (let [{:keys [operation manifest receipt]} (fixture)]
    (doseq [key (keys (publication/identity operation manifest))]
      (is (thrown? clojure.lang.ExceptionInfo (publication/validate-receipt! operation manifest (dissoc receipt key))))
      (is (thrown? clojure.lang.ExceptionInfo (publication/validate-receipt! operation manifest (assoc receipt key ::wrong)))))
    (doseq [bad [(assoc receipt :publication/extra :not-accepted)
                 (assoc receipt :callback (fn [] (throw (AssertionError.))))
                 (update receipt :publication/entries pop)
                 (update receipt :publication/entries conj (first (:publication/entries receipt)))
                 (assoc-in receipt [:publication/entries 0 :publication.entry/path] "/wiki/other.md")
                 (assoc-in receipt [:publication/entries 0 :publication.entry/state] :deleted)
                 (assoc-in receipt [:publication/entries 0 :publication.entry/content :geschichte.content/id] (random-uuid))
                 (assoc-in receipt [:publication/entries 0 :publication.entry/size] 999)
                 (assoc-in receipt [:publication/entries 0 :publication.entry/mode] 33261)
                 (assoc-in receipt [:publication/entries 0 :publication.entry/extra] true)]]
      (is (thrown? clojure.lang.ExceptionInfo (publication/validate-receipt! operation manifest bad))))))

(deftest outcomes-are-exact-and-noop-has-an-original-nonempty-head
  (let [{:keys [operation manifest receipt]} (fixture)
        noop (-> receipt (dissoc :publication/commit) (assoc :publication/noop? true))]
    (is (= noop (publication/validate-receipt! operation manifest noop)))
    (doseq [bad [(assoc receipt :publication/head (random-uuid))
                 (dissoc receipt :publication/commit) (dissoc receipt :publication/head)
                 (assoc receipt :publication/noop? nil) (assoc receipt :publication/created-at "today")
                 (assoc noop :publication/commit nil) (dissoc noop :publication/head)]]
      (is (thrown? clojure.lang.ExceptionInfo (publication/validate-receipt! operation manifest bad))))
    (let [accepted (assoc operation :state :accepted :commit (:publication/commit receipt)
                          :head (:publication/head receipt) :noop? false)]
      (is (= receipt (publication/validate-receipt! accepted manifest receipt)))
      (let [other (random-uuid)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (publication/validate-receipt! (assoc accepted :commit other :head other) manifest receipt)))))))
