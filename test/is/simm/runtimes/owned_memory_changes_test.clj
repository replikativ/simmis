(ns is.simm.runtimes.owned-memory-changes-test
  "Inactive notifier tests: real canonical ownership/EACL/two native systems.
   Only filesystem-backed default provisioning uses the existing memory seam."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.room.registry :as registry]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.owned-workspace-test :as fixture]
            [is.simm.runtimes.memory-changes :as changes]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit CountDownLatch]))

(defn- with-fixture [owned-first? f]
  (let [configs (atom []) chat-conn (atom nil) completed? (atom false)]
    (try
      (let [result (@#'fixture/with-fixture
                    owned-first?
                    (fn [{:keys [conn room owned attached] :as data}]
                      (reset! chat-conn (:conn (:store room)))
                      (reset! configs (mapv #(-> % deref :config) [conn owned attached @chat-conn]))
                      (f data)))]
        (reset! completed? true)
        result)
      (finally
        ;; Normal return proves the shared physical teardown returned. If any
        ;; callback/teardown throws, retain captured stores for diagnosis rather
        ;; than infer quiescence or delete possibly live fixture resources.
        (when @completed?
        ;; The shared fixture has already physically closed its Room and
        ;; released control/workspace connections. Its separate chat store was
        ;; ungoverned but remains ours to release after physical Room close.
          (doseq [cfg @configs]
            (when-not (and (= :memory (get-in cfg [:store :backend]))
                           (uuid? (get-in cfg [:store :id])))
              (throw (ex-info "Refuse non-fixture cleanup" {}))))
          (when @chat-conn (d/release @chat-conn))
          (doseq [cfg @configs]
            (d/delete-database cfg)))))))

(defn- take-hint [^LinkedBlockingQueue queue]
  (.poll queue 5000 TimeUnit/MILLISECONDS))

(defn- source-listeners [registrations conn]
  (filter #(and (identical? conn (:conn %)) (= ::changes/owned-source (first (:key %)))) @registrations))

(defn- capture-listeners [registrations]
  (let [listen d/listen-commits]
    (fn [conn key callback]
      (swap! registrations conj {:conn conn :key key :callback callback})
      (listen conn key callback))))

(defn- join-physically! [f]
  (let [interrupted? (volatile! false)]
    (try
      (loop []
        (when-not (try (f) true
                       (catch InterruptedException _ (vreset! interrupted? true) false))
          (recur)))
      (finally (when @interrupted? (.interrupt (Thread/currentThread)))))))

(deftest snapshot-basis-fences-shared-writer-wrappers-and-real-basis-changes
  (with-fixture
    true
    (fn [{:keys [owned attached]}]
      (let [before @owned repeated @owned same? @#'changes/same-snapshot?]
        ;; The actual shared writer returns non-identical wrappers on this pin.
        (is (not (identical? before repeated)))
        (is (same? before repeated))
        (is (not (same? before @attached)))
        (is (not (same? before (assoc before :max-tx (inc (:max-tx before))))))
        (is (not (same? before (assoc-in before [:config :store :id] (random-uuid)))))
        (is (not (same? before (assoc before :config nil))))
        (is (not (same? before (assoc-in before [:meta :datahike/commit-id] nil))))
        (is (not (same? before (assoc-in before [:meta :datahike/id] nil))))
        (repo/write! owned "wiki/basis.md" (markdown/source-bytes "Actual transaction"))
        (is (not (same? before @owned)))
        (repo/branch! owned "snapshot-checkout")
        (let [same-head @owned]
          (repo/checkout! owned "snapshot-checkout")
          (is (not (same? same-head @owned))))))))

(deftest owned-target-replacement-rebinds-even-identical-bytes-in-both-orders
  (doseq [owned-first? [true false]]
    (with-fixture
      owned-first?
      (fn [{:keys [conn w room owned attached attached-descriptor]}]
       ;; Exact identical worktree bytes do not imply the same owned target.
        (@#'fixture/seed! attached "# Owned\n")
        (let [registrations (atom []) hints (LinkedBlockingQueue.)]
          (with-redefs [d/listen-commits (capture-listeners registrations)]
            (let [owner (changes/open-owned! conn (:ctx room) #(.put hints [%1 %2]))]
              (try
                (let [ack ((:watch! owner) (:room w) room (:owner w))
                      old (first (source-listeners registrations owned))
                      branch (get-in @owned [:config :branch])]
                  (is (uuid? (:target-stamp ack)))
                  (is (= [(:owner w) {:kind :memory-changed :room-id (:room w)}] (take-hint hints)))
                  (is (= 1 ((:watch-count owner))))
                  (is (= 1 (count (source-listeners registrations owned))))
                  (is (empty? (source-listeners registrations attached)))
                  (is (false? ((:callback old) {:branch :unrelated-fork})))
                 ;; An unregistered same-id hook is an invalidation only. It
                 ;; must not detach the still-canonical live source or become
                 ;; an accidental selector while awaiting another real hook.
                  ((:refresh-room! owner) (assoc room :not-canonical true))
                  (is (some? (take-hint hints)))
                  (is (= 1 (count (source-listeners registrations owned))))
                  (is (contains? @(:commit-listeners (meta owned)) (:key old)))
                  (is (true? (:watching? ((:watch! owner) (:room w) room (:owner w)))))
                  (is (some? (take-hint hints)))
                  (d/transact conn [{:room/id (:room w)
                                     :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                  (is (= [(:owner w) {:kind :memory-changed :room-id (:room w)}] (take-hint hints)))
                  (let [next (last (source-listeners registrations attached))
                        rebound ((:watch! owner) (:room w) room (:owner w))]
                    (is (some? next))
                    (is (not= (:target-stamp ack) (:target-stamp rebound)))
                    (is (= 1 ((:watch-count owner))))
                    (is (not (contains? @(:commit-listeners (meta owned)) (:key old))))
                    (is (false? ((:callback old) {:branch branch})))
                    (is (false? ((:callback next) {:branch :unrelated-fork})))
                   ;; Consume the explicit re-watch's initial hint BEFORE
                   ;; asserting a subsequent actual durable-write notification.
                    (is (some? (take-hint hints)))
                   ;; A real durable write to the new owned target wakes the
                   ;; same private room stream, regardless of ambient selection.
                    (repo/write! attached "wiki/new.md" (markdown/source-bytes "Current owned write\n"))
                    (is (= [(:owner w) {:kind :memory-changed :room-id (:room w)}] (take-hint hints)))))
                (finally ((:close! owner))))
              (is (not ((:running? owner))))
              (doseq [{:keys [conn key callback]} @registrations
                      :when (#{::changes/owned-source ::changes/owned-control} (first key))]
                (is (not (contains? @(:commit-listeners (meta conn)) key)))))))))))

(deftest unavailable-target-still-notifies-readers-and-revocation-detaches
  (with-fixture
    true
    (fn [{:keys [conn w room owned]}]
      (let [registrations (atom []) hints (LinkedBlockingQueue.)]
        (with-redefs [d/listen-commits (capture-listeners registrations)]
          (let [owner (changes/open-owned! conn (:ctx room) #(.put hints [%1 %2]))]
            (try
              ((:watch! owner) (:room w) room (:member w))
              (is (some? (take-hint hints)))
              (let [old (last (source-listeners registrations owned))]
                (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository]])
                (is (= [(:member w) {:kind :memory-changed :room-id (:room w)}] (take-hint hints)))
                (is (zero? ((:watch-count owner))))
                (is (false? ((:callback old) {:branch (get-in @owned [:config :branch])})))
               ;; Control observation survives missing targets and can follow a
               ;; future authorized replacement without a write to the old repo.
                (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:repo-sys w)]}])
                (is (some? (take-hint hints)))
                (is (= 1 ((:watch-count owner)))))
              (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
             ;; watch! performs its own current authority check synchronously.
              (is (thrown? clojure.lang.ExceptionInfo ((:watch! owner) (:room w) room (:member w))))
             ;; The private transport interest remains, but cannot authorize a
             ;; hint. Wait for fresh positive reconciliation, not a delay.
             ;; A second allowed reader gives a positive worker barrier;
             ;; its reconciliation excludes the revoked reader from delivery.
              ((:watch! owner) (:room w) room (:owner w))
              (let [hint (take-hint hints)]
                (is (= (:owner w) (first hint)))
                (is (= #{(:owner w) (:member w)} (get-in ((:interests owner)) [(:room w) :readers]))))
             ;; No explicit re-watch: private transport interest survives the
             ;; denial, but only a real current EACL regrant permits delivery.
              (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]}])
              (let [seen (loop [seen #{} remaining 5]
                           (if (or (= #{(:owner w) (:member w)} seen) (zero? remaining))
                             seen
                             (recur (conj seen (first (take-hint hints))) (dec remaining))))]
                (is (= #{(:owner w) (:member w)} seen)))
              (finally ((:close! owner))))))))))

(deftest sole-reader-revocation-detaches-and-regrant-rebinds-without-rewatch
  (with-fixture
    true
    (fn [{:keys [conn w room owned]}]
      (let [registrations (atom []) hints (LinkedBlockingQueue.) detached (promise)
            unlisten d/unlisten-commits]
        (with-redefs [d/listen-commits (capture-listeners registrations)]
          (let [owner (changes/open-owned! conn (:ctx room) #(.put hints [%1 %2]))]
            (try
              ((:watch! owner) (:room w) room (:member w))
              (is (= (:member w) (first (take-hint hints))))
              (let [old (first (source-listeners registrations owned))]
                (with-redefs [d/unlisten-commits
                              (fn [c key]
                                (let [result (unlisten c key)]
                                  (when (and (identical? c owned) (= key (:key old)))
                                    (deliver detached true))
                                  result))]
                  (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                  ;; Positive physical listener-removal ACK, not a sleep or a
                  ;; claim that an empty notification queue proves revocation.
                  (is (= true (deref detached 5000 ::timeout)))
                  (is (not (contains? @(:commit-listeners (meta owned)) (:key old)))))
                (is (= #{(:member w)} (get-in ((:interests owner)) [(:room w) :readers])))
                (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]}])
                (is (= [(:member w) {:kind :memory-changed :room-id (:room w)}] (take-hint hints)))
                (is (= 1 ((:watch-count owner))))
                (is (= 2 (count (source-listeners registrations owned))))
                (is (false? ((:callback old) {:branch (get-in @owned [:config :branch])}))))
              (finally ((:close! owner))))))))))

(deftest control-change-during-resolution-does-not-install-the-old-binding
  (with-fixture
    false
    (fn [{:keys [conn w room owned attached attached-descriptor]}]
      (let [registrations (atom []) hints (LinkedBlockingQueue.)
            entered (promise) release (promise) held? (atom false) resolve owned/resolve!]
        (with-redefs [d/listen-commits (capture-listeners registrations)
                      owned/resolve!
                      (fn [& args]
                        (let [result (apply resolve args)]
                          (when (and (= "simmis-owned-memory-changes" (.getName (Thread/currentThread)))
                                     (compare-and-set! held? false true))
                            (deliver entered true)
                            @release)
                          result))]
          (let [owner (changes/open-owned! conn (:ctx room) #(.put hints [%1 %2]))]
            (try
              ((:watch! owner) (:room w) room (:owner w))
              (is (= true (deref entered 5000 ::timeout)))
             ;; This transaction must return while owned resolution is held:
             ;; neither its commit callback nor the lifecycle lock may resolve.
              (d/transact conn [{:room/id (:room w)
                                 :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
              (deliver release true)
              (is (some? (take-hint hints)))
              (is (= 1 ((:watch-count owner))))
              (is (seq (source-listeners registrations attached)))
              (let [old (first (source-listeners registrations owned))]
                (is (not (contains? @(:commit-listeners (meta owned)) (:key old)))))
              (finally
                (deliver release true)
                ((:close! owner))))))))))

(deftest retiring-room-and-close-fence-callbacks-and-join-the-physical-worker
  (with-fixture
    true
    (fn [{:keys [conn w room owned]}]
      (let [registrations (atom []) entered (promise) release (promise)
            physically-exited (promise)]
        (with-redefs [d/listen-commits (capture-listeners registrations)]
          (let [owner (changes/open-owned!
                       conn (:ctx room)
                       (fn [& _]
                         (deliver entered true)
                        ;; Ignore a cancellation signal until this owned native
                        ;; publisher exits; close must actually JOIN it.
                         (try (join-physically! #(deref release))
                              (finally (deliver physically-exited true)))))]
            (try
              ((:watch! owner) (:room w) room (:owner w))
              (is (= true (deref entered 5000 ::timeout)))
              (let [source (first (source-listeners registrations owned))]
                ((:unwatch-room! owner) room)
               ;; An unregistered same-id hook must not erase the exact retiring
               ;; incarnation before worker-side canonical verification.
                ((:refresh-room! owner) (assoc room :not-canonical true))
                (is (identical? room (get-in ((:interests owner)) [(:room w) :retired-room])))
                (is (zero? ((:watch-count owner))))
                (is (false? ((:callback source) {:branch (get-in @owned [:config :branch])})))
                (is (thrown? clojure.lang.ExceptionInfo ((:watch! owner) (:room w) room (:owner w))))
                (let [close (future ((:close! owner)))]
                  (try
                    (is (= ::still-live (deref close 25 ::still-live)))
                    (is (not (realized? physically-exited)))
                    (deliver release true)
                    (is (nil? (deref close 5000 ::timeout)))
                    (is (= true (deref physically-exited 1000 ::timeout)))
                    (is (not ((:running? owner))))
                    (finally
                      (deliver release true)
                      (join-physically! #(deref close))))))
              (finally
                (deliver release true)
                ((:close! owner))))))))))

(deftest post-install-resolution-refuses-a-logical-reference-switch-without-control-write
  (with-fixture
    true
    (fn [{:keys [conn w room owned]}]
      (let [hints (LinkedBlockingQueue.) registrations (atom [])
            listen d/listen-commits
            changed? (atom false) control-before @conn]
       ;; The native resolver and source changes stay real. Switch the checkout
       ;; AFTER actual listener registration, without changing the control DB.
       ;; Its commit callback must only enqueue, even in this installation gap.
        (with-redefs [d/listen-commits
                      (fn [c key callback]
                        (swap! registrations conj {:conn c :key key :callback callback})
                        (let [result (listen c key callback)]
                          (when (and (identical? c owned) (= ::changes/owned-source (first key))
                                     (compare-and-set! changed? false true))
                            (repo/branch! owned "post-install-checkout")
                            (repo/checkout! owned "post-install-checkout"))
                          result))]
          (let [owner (changes/open-owned! conn (:ctx room) #(.put hints [%1 %2]))]
            (try
              (is (thrown? clojure.lang.ExceptionInfo ((:watch! owner) (:room w) room (:owner w))))
              (is (= [(:owner w) {:kind :memory-changed :room-id (:room w)}] (take-hint hints)))
              (let [[old current] (source-listeners registrations owned)]
                (is (some? old))
                (is (some? current))
                (is (false? ((:callback old) {:branch (get-in @owned [:config :branch])})))
                (is (not (contains? @(:commit-listeners (meta owned)) (:key old)))))
              (is (= control-before @conn))
              (finally ((:close! owner))))))))))

(deftest canonical-room-refresh-rebinds-with-no-control-database-write
  (with-fixture
    false
    (fn [{:keys [conn w room owned]}]
      (let [registrations (atom []) hints (LinkedBlockingQueue.) replacement (atom nil)
            rebound-hint (promise)]
        (with-redefs [d/listen-commits (capture-listeners registrations)]
          (let [owner (changes/open-owned!
                       conn (:ctx room)
                       (fn [party hint]
                         (.put hints [party hint])
                         ;; Retirement legitimately emits an unavailable hint.
                         ;; Only a publication AFTER the replacement's new exact
                         ;; source key is attached proves automatic rebinding.
                         (let [sources (source-listeners registrations owned)
                               latest (last sources)]
                           (when (and @replacement (< 1 (count sources))
                                      (contains? @(:commit-listeners (meta owned)) (:key latest)))
                             (deliver rebound-hint [party hint])))))]
            (try
              ((:watch! owner) (:room w) room (:owner w))
              (is (some? (take-hint hints)))
              (let [control-before @conn old (last (source-listeners registrations owned))]
               ;; Real lifecycle operations, not a Room facade or temporary
               ;; selection change. The existing fixture ultimately closes its
               ;; original root context after this replacement has quiesced.
                ((:unwatch-room! owner) room)
                (registry/unregister! (:id room))
                (let [next (discourse/make-room {:id (:id room) :ctx (:ctx room) :store (:store room)})]
                  (reset! replacement next)
                  (registry/register! next)
                  ((:refresh-room! owner) next)
                  (is (= [(:owner w) {:kind :memory-changed :room-id (:room w)}]
                         (deref rebound-hint 5000 ::timeout)))
                  (is (= 1 ((:watch-count owner))))
                  (is (not (contains? (get ((:interests owner)) (:room w)) :retired-room)))
                  (is (= (:id next) (get-in ((:interests owner)) [(:room w) :native-id])))
                  (is (thrown? clojure.lang.ExceptionInfo ((:watch! owner) (:room w) room (:owner w))))
                  (is (true? (:watching? ((:watch! owner) (:room w) next (:owner w)))))
                  (is (false? ((:callback old) {:branch (get-in @owned [:config :branch])})))
                  (is (= control-before @conn))))
              (finally
                ((:close! owner))
                (when @replacement (discourse/close-room! @replacement {:close-context? false}))))))))))

(deftest activation-seam-never-silently-reuses-another-mode-or-held-owner
  (with-fixture
    true
    (fn [{:keys [conn w room]}]
     ;; Scope an EMPTY isolated process-owner slot; never overwrite a running
     ;; application service in the shared REPL or create a parallel notifier.
      (when @(var-get #'changes/service)
        (throw (ex-info "Activation fixture requires no active global notifier" {})))
      (with-redefs-fn
        {#'changes/service (atom nil)}
        (fn []
          (let [publish (fn [& _])
                legacy {:resolve-room (constantly room) :allowed? (constantly true) :publish! publish}]
            (try
              (changes/start! legacy)
              (is (thrown? clojure.lang.ExceptionInfo (changes/watch-owned! (:room w) room (:owner w))))
              (is (thrown? clojure.lang.ExceptionInfo (changes/start-owned! conn (:ctx room) publish)))
              (changes/stop!)
              (changes/start-owned! conn (:ctx room) publish)
              (is (= :owned (:mode @(var-get #'changes/service))))
              (is (true? (:watching? (changes/watch-owned! (:room w) room (:owner w)))))
              (is (nil? (changes/start-owned! conn (:ctx room) publish)))
              (is (thrown? clojure.lang.ExceptionInfo (changes/start! legacy)))
              (is (thrown? clojure.lang.ExceptionInfo (changes/start-owned! conn (:ctx room) (fn [& _]))))
              ((:close! @(var-get #'changes/service)))
              (is (thrown? clojure.lang.ExceptionInfo (changes/start-owned! conn (:ctx room) publish)))
              (finally (changes/stop!)))))))))
