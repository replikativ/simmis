(ns is.simm.documents.saved-view-reader-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.saved-view-fences :as fences]
            [is.simm.documents.saved-view-reader :as reader]
            [is.simm.documents.page-views :as views]
            [is.simm.documents.page-views-test :as fixture]
            [is.simm.documents.saved-view-spec-test :as declarations]))

(defn- fence [descriptor] (str "```simmis-view\n" (pr-str descriptor) "\n```\n"))
(defn- write! [conn text] (repo/write! conn "views/report.md" (markdown/source-bytes text)))

(defn- request [source]
  (let [file (files/read-file-in source "views/report.md")
        view (first (get-in file [:analysis :saved-views]))]
    (merge {:path "views/report.md" :expected (:entry file) :revision (get-in file [:analysis :revision])}
           (select-keys view [:from :to :view]))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest exact-saved-fence-selects-inert-data-and-preserves-source
  (@#'fixture/with-fixture
   (fn [{:keys [conn descriptor]}]
     (let [source (str "# Report\n\n" (fence descriptor))]
       (write! conn source)
       (let [database @conn tx (:max-tx database) sent (request database)
             result (reader/inspect-pages-in database sent)]
         (is (= :ready (:status result)))
         (is (= (:id descriptor) (:view result)))
         (is (= [fixture/page-a fixture/page-b] (mapv :id (:rows result))))
         (is (= sent (select-keys result (keys sent))))
         (is (= source (:text (files/read-file-in @conn "views/report.md"))))
         (is (= tx (:max-tx @conn)))
         (with-redefs [views/prepare (fn [& _] (throw (AssertionError. "No query for a forged fence")))]
           (doseq [override [{:from (inc (:from sent))} {:to (inc (:to sent))} {:view (random-uuid)}
                             {:revision (random-uuid)} {:expected (assoc (:expected sent) :content (random-uuid))}]]
             (is (some? (reason #(reader/inspect-pages-in database (merge sent override))))))
           (is (= :invalid-request (reason #(reader/inspect-pages-in database (assoc sent :descriptor descriptor))))))
         (write! conn (str source "\nChanged file\n"))
         (is (= :source-changed (reason #(reader/inspect-pages-in @conn sent)))))))))

(deftest malformed-and-duplicate-declarations-have-no-queryable-partial-identity
  (@#'fixture/with-fixture
   (fn [{:keys [conn descriptor]}]
     (write! conn (str (fence descriptor) "\n" (fence descriptor)))
     (let [file (files/read-file-in @conn "views/report.md")
           entries (get-in file [:analysis :saved-views])]
       (is (= [:invalid :invalid] (mapv :status entries)))
       (is (every? #(= :duplicate-view-identity (:reason %)) entries))
       (with-redefs [views/prepare (fn [& _] (throw (AssertionError. "Duplicate cannot select a query")))]
         (is (= :invalid-or-missing-fence (reason #(reader/inspect-pages-in @conn (request @conn)))))))
     (write! conn (str "```simmis-view\n#=(System/exit 1)\n```\n"))
     (let [file (files/read-file-in @conn "views/report.md")]
       (is (= :invalid (get-in file [:analysis :saved-views 0 :status])))
       (is (= :invalid-declaration (get-in file [:analysis :saved-views 0 :reason])))))))

(deftest nested-fences-use-parser-spans-and-mounted-count-is-bounded
  (@#'fixture/with-fixture
   (fn [{:keys [conn descriptor]}]
     (write! conn (str "> ```simmis-view\n> " (pr-str descriptor) "\n> ```\n"))
     (is (= :ready (:status (reader/inspect-pages-in @conn (request @conn)))))
     (let [descriptors (mapv #(assoc descriptor :id %) (repeatedly 5 random-uuid))]
       (write! conn (apply str (interpose "\n" (map fence descriptors))))
       (let [file (files/read-file-in @conn "views/report.md")
             nodes (filter fences/fence? (get-in file [:analysis :nodes]))
             fifth (nth nodes 4)
             sent (merge (request @conn) (select-keys fifth [:from :to]) {:view (:id (peek descriptors))})]
         (is (= 4 (count (get-in file [:analysis :saved-views]))))
         (with-redefs [views/prepare (fn [& _] (throw (AssertionError. "Unlisted fence must not query")))]
           (is (= :invalid-or-missing-fence (reason #(reader/inspect-pages-in @conn sent))))))))))

(deftest receipt-fence-resolution-never-requires-the-canonical-page-projection
  (@#'fixture/with-fixture
   (fn [{:keys [conn]}]
     (doseq [n (range 201)]
       (repo/write! conn (str "archive/" n ".md") (markdown/source-bytes "# Archived evidence\n")))
     (write! conn (fence declarations/scorecard-table))
     (let [source @conn sent (request source)]
       (with-redefs [views/prepare (fn [& _] (throw (AssertionError. "Receipt opened page projection")))]
         (let [resolved (reader/resolve-in source sent)]
           (is (= (:source declarations/scorecard-table) (get-in resolved [:descriptor :source])))
           (is (= (:view sent) (:view resolved)))
           (is (= (:revision sent) (:revision resolved)))
           (is (not (contains? resolved :rows)))
           (is (not (contains? resolved :db)))))))))
