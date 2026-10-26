(ns is.simm.documents.capture-views-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.capture :as capture]
            [is.simm.documents.capture-test :as data]
            [is.simm.documents.capture-views :as views]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.saved-view-spec :as spec])
  (:import [java.time Instant]))

(defn- with-store [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? false :max-string-length 0 :max-bytes-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Capture view fixture"}) (f conn)
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- write-note! [conn session segment frames]
  (let [note (capture/observation-note session segment frames)]
    (repo/write! conn (:path note) (markdown/source-bytes (:text note)))
    note))

(defn descriptor [session]
  {:version 1 :id (random-uuid) :source {:kind :archived-captures :roots [(str (capture/folder session) "/observations")]}
   :presentation :table :columns [{:kind :started-at} {:kind :screenshots} {:kind :note} {:kind :recording}]})

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest archive-selection-is-explicit-inert-and-readonly
  (let [value (descriptor data/session)]
    (is (= 32 (:limit (spec/validate value))))
    (doseq [bad [(assoc value :presentation :form)
                 (assoc-in value [:source :roots] ["captures"])
                 (assoc-in value [:source :roots] ["../captures"])
                 (assoc-in value [:source :owner] (random-uuid))
                 (assoc-in value [:columns 0] {:property (random-uuid)})
                 (assoc-in value [:source :roots] (vec (repeat 2 (first (get-in value [:source :roots])))))]]
      (is (some? (reason #(spec/validate bad)))))))

(deftest note-intervals-sort-by-time-across-midnight-without-loading-media-or-global-pages
  (with-store
    (fn [conn]
      (let [session (assoc data/session :started-at (.toEpochMilli (Instant/parse "2026-10-25T23:58:00Z")))
            first-note (write-note! conn session (assoc data/segment :index 0 :offset-ms 0 :duration-ms 180000) [])
            next-note (write-note! conn session (assoc data/segment :index 1 :offset-ms 180000 :duration-ms 180000) [])
            source @conn tx (:max-tx source)
            read-bytes files/source-bytes]
        ;; Archive reads do not depend on the 200-file canonical projection.
        (doseq [n (range 201)] (repo/write! conn (str "other/" n ".md") (markdown/source-bytes "# Unrelated\n")))
        (with-redefs [files/source-bytes (fn [db path entry]
                                         (is (str/ends-with? path ".md") "No image/video payload read")
                                         (read-bytes db path entry))]
          (let [result (views/prepare @conn (descriptor session))]
            (is (= :ready (:status result))))
          (let [result (views/prepare @conn (descriptor session))]
            (is (= [(:path first-note) (:path next-note)] (mapv :path (:rows result))))
            (is (str/starts-with? (get-in result [:rows 0 :cells 0 :text]) "2026-10-25T23:58"))
            (is (= "Original unavailable" (get-in result [:rows 0 :cells 3 :text])))
            (is (not-any? #(contains? % :edit) (:rows result))))
          (let [result (views/prepare source (assoc (descriptor session) :limit 1))]
            (is (= 2 (:total result))) (is (= 1 (count (:rows result))))
            (is (false? (:complete? result)))))
        (is (= tx (:max-tx source)) "captured source remains immutable")))))

(deftest raw-index-and-all-selected-byte-limits-precede-body-reading
  (let [seen (atom 0)
        rows (fn rows [n] (lazy-seq (cons (do (swap! seen inc) {:v (str "entry-" n)}) (rows (inc n)))))
        forbidden (fn [& _] (throw (AssertionError. "Overflow reached entry/body expansion")))]
    (with-redefs [views/max-index-entries 2 d/index-range (fn [& _] (rows 0))
                  files/entry forbidden files/source-bytes forbidden]
      (is (= :archive-index-limit (reason #(views/prepare ::db (descriptor data/session)))))
      (is (= 3 @seen))))
  (with-store
    (fn [conn]
      (write-note! conn data/session data/segment [])
      (with-redefs [views/max-source-bytes 1
                    files/source-bytes (fn [& _] (throw (AssertionError. "Byte overflow reached a body")))]
        (is (= :archive-byte-limit (reason #(views/prepare @conn (descriptor data/session)))))))))

(deftest asset-links-require-current-matching-workspace-content
  (with-store
    (fn [conn]
      (let [note (capture/observation-note data/session data/segment [data/frame])
            video-path (:path (first (:assets note))) image-path (:path (second (:assets note)))
            _ (repo/write! conn video-path (byte-array [1 2 3]))
            _ (repo/write! conn image-path (byte-array [4 5 6]))
            video (files/entry @conn video-path) image (files/entry @conn image-path)
            _ (write-note! conn data/session (merge data/segment (select-keys video [:content :size]))
                           [(merge data/frame (select-keys image [:content :size]))])
            result (views/prepare @conn (descriptor data/session))]
        (is (= "1 / 1" (get-in result [:rows 0 :cells 1 :text])))
        (is (= :media (get-in result [:rows 0 :cells 3 :kind])))
        (is (= (:content video) (get-in result [:rows 0 :cells 3 :content])))
        (repo/write! conn video-path (byte-array [7 8 9]))
        (repo/write! conn image-path (byte-array [7 8 9]))
        (let [changed (views/prepare @conn (descriptor data/session))]
          (is (= "0 / 1" (get-in changed [:rows 0 :cells 1 :text])))
          (is (= "Original unavailable" (get-in changed [:rows 0 :cells 3 :text])))
          (is (not= (:basis result) (:basis changed))))))))

(deftest empty-archive-and-zero-duration-producer-notes-are-valid
  (with-store
    (fn [conn]
      (let [empty-result (views/prepare @conn (descriptor data/session))]
        (is (= :ready (:status empty-result))) (is (= 0 (:total empty-result)))
        (is (:complete? empty-result)))
      (write-note! conn data/session (assoc data/segment :duration-ms 0) [])
      (let [result (views/prepare @conn (assoc (descriptor data/session) :columns [{:kind :duration}]))]
        (is (= 1 (:total result)))
        (is (= "0" (get-in result [:rows 0 :cells 0 :text])))))))

(deftest oversized-single-note-is-refused-before-deterministic-parser-allocation
  (with-store
    (fn [conn]
      (let [note (write-note! conn data/session data/segment [])
            eid (:e (first (d/datoms @conn :avet :geschichte.work/path (:path note))))]
        (d/transact conn [[:db/add eid :geschichte.work/size (inc markdown/max-source-bytes)]])
        (with-redefs [files/source-bytes (fn [& _] (throw (AssertionError. "Oversized note body read")))]
          (is (= :invalid-note-metadata (reason #(views/prepare @conn (descriptor data/session))))))))))

(deftest malformed-notes-and-escaping-media-references-never-produce-partial-rows
  (with-store
    (fn [conn]
      (let [note (capture/observation-note data/session data/segment [data/frame])
            source (:text note)]
        (doseq [changed [(str/replace source "screen-observation" "other-source")
                         (str/replace source "estimated" "exact")
                         (str/replace source "/recordings/" "/../recordings/")
                         (str/replace source "source-kind:" "source-kind: [unclosed")]]
          (repo/write! conn (:path note) (markdown/source-bytes changed))
          (is (some? (reason #(views/prepare @conn (descriptor data/session))))))))))
