(ns is.simm.uis.web.desktop.memory-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(deftest passive-refresh-and-reordered-reads-never-navigate-or-resurrect-a-file
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)}
          states (signal runtime {}) replies (atom {})
          defer (fn [slot] (fn [ok _] (swap! replies assoc slot ok)))
          data {:room-id "test-room" :col-id :test :tab-id :file :state-signal states}
          a-file {:path "a.clj" :entry {:content :a} :text "A"}
          b-file {:path "b.clj" :entry {:content :b} :text "B"}]
      (with-redefs [web/current-session (constantly session)
                    sig/memory-connection (signal runtime connection)
                    web/current-session? (constantly true)
                    sync/ensure! (fn [_])
                    remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {})))
                    remote/load-memory-metadata! (fn [& _] (defer :metadata))
                    remote/load-memory-folder! (fn [& _] (defer :folder))
                    remote/load-memory-file! (fn [_ _ path] (defer [:file path]))]
        (let [a (actions/controller data) key (:owner a) stream {:connection connection :status :ready :dirty {}}
              state #(get @states key)]
          (swap! states assoc key {:selected "a.clj" :folders {"" {:entries []}}
                                   :drafts {"a.clj" (editor/draft a-file) "b.clj" (editor/draft b-file)}})
          ((:folder a) "slow")
          (let [old-folder (:folder @replies)]
            ((:ensure-sync a) connection stream)
            ((:ensure-sync a) connection stream)
            ((:metadata @replies) {:folders {"" {:entries [:new]} "slow" {:entries [:current]}}
                                   :entries {"a.clj" (:entry a-file) "b.clj" {:content :new-b}} :revision :v1})
            (is (= "a.clj" (:selected (state))) "Background B refresh does not select B")
            (old-folder {:entries [:stale]})
            (is (= [:current] (get-in (state) [:folders "slow" :entries])))
            ((:reload a) "a.clj")
            (let [old-file (get @replies [:file "a.clj"])]
              ((:ensure-sync a) connection (assoc-in stream [:dirty "test-room"] 1))
              ((:metadata @replies) {:folders {"" {:entries []} "slow" {:entries []}}
                                     :entries {"a.clj" nil "b.clj" (:entry b-file)} :revision :v2})
              (old-file a-file)
              (is (:remote-deleted? (get-in (state) [:drafts "a.clj"])))
              (is (= "A" (get-in (state) [:drafts "a.clj" :text])))
              (is (= "a.clj" (:selected (state)))))))))))

(deftest invalidation-during-a-read-is-reconciled-on-completion-not-by-a-timer
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)}
          states (signal runtime {}) pending (atom nil) calls (atom 0)]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    sync/ensure! (fn [_])
                    remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {})))
                    remote/load-memory-metadata! (fn [& _] (fn [ok _] (swap! calls inc) (reset! pending ok)))]
        (let [a (actions/controller {:room-id "test-room" :col-id :test :state-signal states})
              stream {:connection connection :status :ready :dirty {}}
              later (assoc-in stream [:dirty "test-room"] 1)]
          ((:ensure-sync a) connection stream)
          ((:ensure-sync a) connection stream)
          ((:ensure-sync a) connection later)
          (is (= 1 @calls))
          (@pending {:folders {} :entries {} :revision :one})
          ((:ensure-sync a) connection later)
          (is (= 2 @calls))
          (@pending {:folders {} :entries {} :revision :two})
          ((:ensure-sync a) connection later)
          (is (= 2 @calls)))))))

(deftest initial-file-reply-before-newer-metadata-does-not-strand-an-old-buffer
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)}
          states (signal runtime {}) replies (atom {}) loads (atom 0)]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    sync/ensure! (fn [_])
                    remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {})))
                    remote/load-memory-metadata! (fn [& _] (fn [ok _] (swap! replies assoc :metadata ok)))
                    remote/load-memory-file! (fn [& _] (fn [ok _] (swap! loads inc) (swap! replies assoc :file ok)))]
        (let [a (actions/controller {:room-id "test-room" :col-id :test :state-signal states})
              stream {:connection connection :status :ready :dirty {}}]
          ((:open a) "a.clj")
          (let [initial (:file @replies)]
            ((:ensure-sync a) connection stream)
            ((:ensure-sync a) connection stream)
            (initial {:path "a.clj" :entry {:content :old} :text "old"})
            ((:metadata @replies) {:folders {} :entries {"a.clj" {:content :new}} :revision :new})
            (is (= 2 @loads) "The newer metadata must trigger a fresh body read")
            ((:file @replies) {:path "a.clj" :entry {:content :new} :text "new"})
            (is (= "new" (get-in @states [(:owner a) :drafts "a.clj" :text])))))))))

(deftest a-file-started-after-the-metadata-snapshot-is-not-stranded
  (doseq [body-first? [false true]]
    (binding [ec/*execution-context* runtime]
      (let [session {:id (random-uuid)} connection {:epoch (random-uuid)}
            states (signal runtime {}) replies (atom {}) loads (atom [])
            file {:path "uploaded.clj" :entry {:content :uploaded} :text "Uploaded"}]
        (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                      sig/memory-connection (signal runtime connection)
                      sync/ensure! (fn [_])
                      remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {})))
                      remote/load-memory-metadata!
                      (fn [_ _ request]
                        (is (not (some #{"uploaded.clj"} (:paths request))))
                        (fn [ok _] (swap! replies assoc :metadata ok)))
                      remote/load-memory-file!
                      (fn [& _] (fn [ok _] (swap! loads conj ok)))]
          (let [a (actions/controller {:room-id "test-room" :col-id :test :state-signal states})
                stream {:connection connection :status :ready :dirty {}}
                state #(get @states (:owner a))]
            ((:ensure-sync a) connection stream)
            ((:ensure-sync a) connection stream)
            ;; Like upload completion: this path did not exist when metadata
            ;; captured its request. No selected/deep-link rerender retries it.
            ((:open a) "uploaded.clj")
            ((:select-path a) "other.clj")
            (let [old-body (first @loads)]
              (when body-first? (old-body file))
              ((:metadata @replies) {:folders {} :entries {} :revision :snapshot-before-upload})
              (when-not body-first?
                (is (= 2 (count @loads)) "Metadata must replace the invalidated, omitted body request")
                (old-body (assoc file :text "Stale response"))
                (is (nil? (get-in (state) [:drafts "uploaded.clj"])))
                (is (true? (get-in (state) [:loading [:file "uploaded.clj"]]))
                    "The old callback must not finish its replacement request")
                ((last @loads) file))
              (is (= "Uploaded" (get-in (state) [:drafts "uploaded.clj" :text])))
              (is (= "other.clj" (:selected (state))) "Background retries must not navigate")
              (is (false? (get-in (state) [:loading [:file "uploaded.clj"]]))))))))))
