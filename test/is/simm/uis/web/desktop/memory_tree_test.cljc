(ns is.simm.uis.web.desktop.memory-tree-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.memory-tree :as tree]))

(deftest knowledge-hints-do-not-create-a-second-wiki-drive-or-hide-other-files
  (doseq [path ["wiki/company.md" "notes/research.MARKDOWN" "context.md"]]
    (is (= :knowledge (:role (tree/presentation {:path path :kind :file})))))
  (is (= :knowledge (:role (tree/presentation {:path "wiki" :kind :folder}))))
  (is (= :folder (:role (tree/presentation {:path "wiki/archive" :kind :folder}))))
  (doseq [path ["example.clj" "library.cljc" "app.cljs"]]
    (is (= :code (:role (tree/presentation {:path path :kind :file})))))
  (doseq [path ["budget.xlsx" "important.unknown" "workflow.edn"]]
    (is (= {:role :file :icon "file" :label "File"} (tree/presentation {:path path :kind :file})))))

(deftest deep-paths-can-be-focused-without-renaming-them
  (is (= [{:label "wiki" :path "wiki"} {:label "team" :path "wiki/team"} {:label "研究" :path "wiki/team/研究"}]
         (tree/breadcrumbs "wiki/team/研究")))
  (is (empty? (tree/breadcrumbs "")))
  (is (= 180 (tree/sidebar-width -10 1400)))
  (is (= 640 (tree/sidebar-width 1000 1400)))
  (is (= 325 (tree/sidebar-width 500 500))))

(deftest file-tabs-are-room-and-column-specific
  (let [columns [{:id :a :tabs [{:id :one :type :memory :data {:room-id :room1 :path "a.md"}}
                                {:id :two :type :memory :data {:room-id :room2 :path "a.md"}}]}
                 {:id :b :tabs [{:id :three :type :memory :data {:room-id :room1 :path "a.md"}}]}]]
    (is (= :one (:id (tree/file-tab columns :a :room1 "a.md"))))
    (is (= :two (:id (tree/file-tab columns :a :room2 "a.md"))))
    (is (= :three (:id (tree/file-tab columns :b :room1 "a.md"))))
    (is (nil? (tree/file-tab columns :a :room1 "b.md")))))

(deftest hidden-descendants-do-not-count-towards-active-folder-reconciliation
  (let [state {:root-expanded? true :expanded {"wiki" false "wiki/deep" true "code" true}
               :folders {"" {:entries [{:path "wiki" :kind :folder} {:path "code" :kind :folder}]}
                         "wiki" {:entries [{:path "wiki/deep" :kind :folder}]} "wiki/deep" {} "code" {}}}]
    (is (= #{"" "code"} (tree/active-folders state)))
    (is (= #{"" "wiki/deep"}
           (tree/active-folders (assoc state :focus-folder "wiki/deep"))))
    (is (= #{""} (tree/active-folders (assoc state :root-expanded? false))))
    (is (= #{"" "code"} (set (keys (:folders (tree/invalidate-closed-folders state))))))))

(deftest folder-cache-is-bounded-without-evicting-active-or-pending-listings
  (let [paths (mapv #(str "folder-" %) (range 100))
        state {:root-expanded? true :expanded {"folder-99" true}
               :folder-intents {"folder-98" true}
               :folders (assoc (zipmap paths (repeat {:entries []}))
                               "" {:entries [{:path "folder-99" :kind :folder}]})}
        cached (tree/trim-folder-cache state)]
    (is (= 64 (count (:folders cached))))
    (is (every? #(contains? (:folders cached) %) ["" "folder-99" "folder-98"]))
    (is (= #{"" "folder-99" "folder-98"} (tree/active-folders cached)))))

(deftest deleted-expanded-folders-do-not-remain-pinned-by-expansion-flags
  (let [state {:root-expanded? true :expanded {"deleted" true "deleted/child" true}
               :folders {"" {:entries []}
                         "deleted" {:entries [{:path "deleted/child" :kind :folder}]}
                         "deleted/child" {:entries []}}}]
    (is (= #{""} (tree/active-folders state)))
    (is (= {"" {:entries []}} (:folders (tree/invalidate-closed-folders state))))))
