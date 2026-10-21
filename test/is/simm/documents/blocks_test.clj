(ns is.simm.documents.blocks-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.markdown :as markdown]))

(def document-id #uuid "00000000-0000-4000-8000-000000000001")
(def parent-id #uuid "00000000-0000-4000-8000-000000000002")
(def child-id #uuid "00000000-0000-4000-8000-000000000003")
(defn open [id] (str "<!-- simmis:block " id " -->\n"))
(def close "<!-- /simmis:block -->\n")
(defn project [s] (blocks/project document-id (markdown/parse s)))

(deftest rich-content-and-semantic-children-are-distinct
  (let [source (str (open parent-id) "\nFirst paragraph.\n\nSecond.\n\n- Internal list\n  - Internal child\n\n"
                    (open child-id) "\nChild block.\n\n" close "\nAfter child.\n\n" close)
        result (project source)
        parent (first (:blocks result))
        child (first (:children parent))]
    (is (= :ready (:status result)))
    (is (= 1 (count (:blocks result))))
    (is (= parent-id (:id parent)))
    (is (= child-id (:id child)))
    (is (= 1 (count (:children parent))))
    (is (= [:Paragraph :Paragraph :BulletList :Paragraph] (mapv :kind (:syntax parent))))
    (is (= 2 (count (:regions parent))))
    (is (= 0 (:ordinal child)))
    (doseq [{:keys [from to]} (:regions parent)]
      (is (not (.contains (subs source from to) "Child block."))))))

(deftest markers-in-literal-contexts-do-not-create-identities
  (doseq [source [(str "```html\n" (open parent-id) close "```\n")
                  (str "`" (clojure.string/trim (open parent-id)) "`\n")
                  (str "<!-- example: simmis:block " parent-id " -->\n")]]
    (let [result (project source)]
      (is (= :ready (:status result)))
      (is (every? #(= :provisional (:identity-status %)) (:blocks result))))))

(deftest malformed-and-duplicate-structure-is-not-repaired
  (doseq [[source kind] [[close :unmatched-block-close]
                         [(str (open parent-id) "Unclosed") :unclosed-block]
                         ["<!-- simmis:block not-a-uuid -->\n" :invalid-block-marker]
                         [(str (open parent-id) "a\n\n" close "\n" (open parent-id) "b\n\n" close)
                          :duplicate-block-id]
                         [(str "> " (open parent-id)) :nested-block-marker]]]
    (let [result (project source)]
      (is (= :invalid (:status result)))
      (is (some #(= kind (:kind %)) (:diagnostics result))))))

(deftest selective-parent-edit-preserves-child-and-unknown-source
  (let [source (str "---\nunknown: untouched\n---\n" (open parent-id) "\nBefore.\n\n"
                    (open child-id) "\nChild **rich** body.\n\n" close "\nAfter.\n\n" close)
        revision (markdown/revision source)
        after (blocks/replace-region document-id source revision parent-id 0 "\nRevised.\n\n")
        projected (project (:source after))]
    (is (= :ready (:status projected)))
    (is (.contains (:source after) (str (open child-id) "\nChild **rich** body.\n\n" close)))
    (is (.startsWith (:source after) "---\nunknown: untouched\n---\n"))
    (is (.contains (:source after) "\nAfter.\n\n"))
    (is (thrown? clojure.lang.ExceptionInfo
                 (blocks/replace-region document-id (:source after) revision parent-id 0 "stale")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (blocks/replace-region document-id source revision parent-id 0 close)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (blocks/replace-region document-id source revision parent-id 99 "invalid")))))

(deftest parsing-and-no-op-region-edit-preserve-source
  (doseq [newline ["\n" "\r\n" "\r"]]
    (let [source (clojure.string/replace (str (open parent-id) "\nText.\n\n" close) "\n" newline)
          before (markdown/parse source)
          result (blocks/project document-id before)
          {:keys [from to]} (get-in result [:blocks 0 :regions 0])]
      (is (= :ready (:status result)))
      (is (= source (:source (blocks/replace-region document-id source (:revision before)
                                                   parent-id 0 (subs source from to))))))))

(deftest malformed-metadata-and-excessive-depth-do-not-produce-ready-structure
  (is (= :invalid (:status (project "---\nvalue: [\n---\nbody"))))
  (is (thrown? clojure.lang.ExceptionInfo
               (project (str (apply str (repeat 101 (str (open parent-id) "\n")))
                             (apply str (repeat 101 (str close "\n"))))))))

(deftest region-edits-cannot-become-document-metadata-edits
  (let [source "Hello\n"
        block-id (get-in (project source) [:blocks 0 :id])]
    (is (thrown? clojure.lang.ExceptionInfo
                 (blocks/replace-region document-id source (markdown/revision source) block-id 0
                                        (str "---\nsimmis-id: '" child-id "'\n---\nRebound identity\n"))))))

(deftest region-edits-cannot-hide-a-child-in-a-new-code-fence
  (let [source (str (open parent-id) "\nBefore\n\n" (open child-id) "\nChild\n\n" close
                    "\nAfter\n\n" close)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (blocks/replace-region document-id source (markdown/revision source) parent-id 0
                                        "\n```html\n")))))
