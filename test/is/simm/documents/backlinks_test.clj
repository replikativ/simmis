(ns is.simm.documents.backlinks-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.backlinks :as backlinks]
            [is.simm.documents.block-reference :as references]
            [is.simm.documents.block-reference-test :as fixture]
            [is.simm.documents.links :as links]
            [is.simm.documents.markdown :as markdown]
            [jsonista.core :as json]))

(def room "39bb0b7b-938d-46d7-8a96-5e2f1061c5a5")
(def block #uuid "9ec3553a-8879-4056-8e48-28593b9e2815")
(def parent #uuid "e2f38e61-d095-4af6-9e26-dd51d575814e")
(def child #uuid "c122c4df-7c50-41f9-b046-634cc34395a4")
(defn- group [id body] (str "<!-- simmis:block " id " -->\n\n" body "\n\n<!-- /simmis:block -->\n\n"))
(defn- document [id body] (str "---\nsimmis-id: " id "\n---\n\n" body))
(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))
(defn- query [db target] {:repository-id (:repository-id (references/selection db)) :target target})
(defn- file-query [db path] (query db {:kind :file :path path}))
(defn- setup! [conn]
  (write! conn "wiki/A.md" (document (random-uuid) (group block "Target")))
  (let [repository (:repository-id (references/selection @conn))
        file-url (links/route {:kind :memory-file :room room :path "wiki/A.md"})
        block-url (links/route {:kind :memory-block :room room :repository repository :block block :path "stale/path.md"})
        text (document (random-uuid)
                       (str (group parent (str "Parent.\n\n" (group child (str "[Block](" block-url ")"))))
                            "[File](" file-url ") and [Reference][ref].\n\n"
                            "[ref]: <" file-url ">\n[unused]: <" file-url ">\n\n"
                            "`[Code](" file-url ")`\n\n```markdown\n[Fake](" file-url ")\n```\n\n"
                            "<!-- [Comment](" file-url ") -->\n"))]
    (write! conn "wiki/B.md" text)
    {:file-url file-url :block-url block-url :source text}))

(deftest real-source-backlinks-are-revision-bound-and-find-the-innermost-origin
  (@#'fixture/with-repository
   (fn [conn]
     (let [{:keys [source]} (setup! conn) db @conn
           tx (:max-tx db) selected (references/selection db)
           page (backlinks/selected db selected room (file-query db "wiki/A.md"))
           precise (backlinks/in db room (query db {:kind :block :id block}))]
       (is (= :ready (:status page)))
       (is (:complete? page))
       (is (= 3 (count (:results page))))
       (is (= 1 (count (:results precise))))
       (is (= child (get-in precise [:results 0 :block-id])))
       (is (= (str child) (:block (links/parse-route (get-in precise [:results 0 :url])))))
       (is (= ["wiki/B.md" "wiki/B.md" "wiki/B.md"] (mapv :path (:results page))))
       (is (= ["[Block]" "[File]" "[Reference]"]
              (mapv (fn [{:keys [from to]}]
                      (first (str/split (subs source from to) #"(?:\(|\[ref\])"))) (:results page))))
       (is (every? #(= (markdown/revision source) (:revision %)) (:results page)))
       (is (= tx (:max-tx @conn)))
       (is (not (contains? (d/schema @conn) :document/id)))
       (is (not-any? #(contains? % :parsed) (:results page)))))))

(deftest block-destinations-follow-explicit-identity-and-ignore-path-hints
  (@#'fixture/with-repository
   (fn [conn]
     (setup! conn)
     (write! conn "wiki/A.md" (document (random-uuid) "The original page remains."))
     (write! conn "wiki/Moved.md" (document (random-uuid) (group block "Moved target")))
     (let [result (backlinks/in @conn room (query @conn {:kind :block :id block}))]
       (is (= "wiki/Moved.md" (get-in result [:target :path])))
       (is (= 1 (count (:results result))))
       (is (= 1 (count (:results (backlinks/in @conn room (file-query @conn "wiki/Moved.md")))))))
     (write! conn "wiki/Duplicate.md" (document (random-uuid) (group block "Duplicate target")))
     (is (thrown? clojure.lang.ExceptionInfo (backlinks/in @conn room (query @conn {:kind :block :id block}))))
     (is (= :ambiguous (:status (references/resolve-in @conn {:repository-id (:repository-id (references/selection @conn))
                                                            :block-id block})))))))

(deftest external-relative-and-frozen-input-spellings-do-not-become-guessed-local-links
  (@#'fixture/with-repository
   (fn [conn]
     (let [{:keys [file-url]} (setup! conn)]
       (write! conn "wiki/Other.md"
               (document (random-uuid)
                         (str "[External](https://another.example" file-url ")\n"
                              "[Relative](A.md) [[A]]\n"
                              "[Frozen](/in/knowledge/sources/wiki/A.md)\n")))
       (is (= 3 (count (:results (backlinks/in @conn room (file-query @conn "wiki/A.md"))))))
       (is (= :missing (:status (backlinks/in @conn room (file-query @conn "wiki/Missing.md")))))))))

(deftest scan-basis-rejects-stale-relevant-source-and-ignores-unrelated-data
  (@#'fixture/with-repository
   (fn [conn]
     (setup! conn)
     (let [db @conn selected (references/selection db) q (file-query db "wiki/A.md")
           before (backlinks/selected db selected room q)]
       (write! conn "data/not-a-page.json" "{}")
       (is (= before (backlinks/selected @conn selected room q)))
       (write! conn "wiki/New.md" (document (random-uuid) "New page"))
       (is (thrown? clojure.lang.ExceptionInfo (backlinks/selected @conn selected room q)))
       (is (= before (backlinks/selected db selected room q)))
       (is (not= (:source-basis before) (:source-basis (backlinks/in @conn room q))))))))

(deftest result-count-and-utf8-byte-limits-are-explicitly-incomplete
  (@#'fixture/with-repository
   (fn [conn]
     (let [{:keys [file-url]} (setup! conn) q (file-query @conn "wiki/A.md")]
       (with-redefs [backlinks/profile (assoc backlinks/profile :max-results 1)]
         (let [reply (backlinks/in @conn room q)]
           (is (= :limited (:status reply))) (is (false? (:complete? reply)))
           (is (= 1 (count (:results reply))))))
       (repo/remove! conn "wiki/B.md")
       (write! conn (str "wiki/" (apply str (repeat 200 "研")) ".md")
               (document (random-uuid) (str "[Target](" file-url ")")))
       (with-redefs [backlinks/profile (assoc backlinks/profile :max-reply-bytes 1024)]
         (let [reply (backlinks/in @conn room q)]
           (is (= :limited (:status reply))) (is (false? (:complete? reply)))
           (is (empty? (:results reply)))
           (is (<= (alength ^bytes (json/write-value-as-bytes reply)) 1024))))
       (with-redefs [backlinks/profile (assoc backlinks/profile :max-links 0)]
         (is (thrown? clojure.lang.ExceptionInfo (backlinks/in @conn room q))))))))

(deftest requests-name-only-markdown-files-or-explicit-blocks
  (let [repository (random-uuid)]
    (is (= {:repository-id repository :target {:kind :file :path "wiki/Page.md"}}
           (backlinks/request! {:repository-id repository :target {:kind :file :path "wiki/Page.md"}})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (backlinks/request! {:repository-id repository :target {:kind :file :path "data/chart.json"}})))))
