(ns is.simm.uis.web.desktop.memory-draft-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.memory-editor :as editor]))

(deftest refreshing-a-reverted-draft-does-not-hide-a-successful-pending-save
  (let [file {:path "a.md" :entry {:content :a} :text "A"}
        opened (editor/draft file)
        typed (editor/changed opened "B" {:anchor 1 :head 1})
        reverted (editor/changed typed "A" {:anchor 1 :head 1})
        refreshed (editor/loaded reverted file)
        ack (editor/saved-at-generation refreshed (:generation typed) "B" {:entry {:content :b}})]
    (is (= (:generation typed) (:generation refreshed)))
    (is (= "A" (:text ack)))
    (is (= "B" (get-in ack [:source :text])))
    (is (= {:content :b} (get-in ack [:source :entry])))
    (is (editor/dirty? ack))))

(deftest unknown-mixed-yaml-keys-remain-displayable
  (let [properties {"title" "Readable" 7 "numeric key" false "boolean key" {"custom" "key"} "structured key"}
        entries (editor/property-entries properties)]
    (is (= properties (into {} entries)))
    (is (= 4 (count entries)))
    (is (every? string? (map (comp pr-str key) entries)))))

(deftest code-views-follow-user-preference-with-a-local-override
  (is (= :clojure (editor/code-view {} nil)))
  (is (= :superficie (editor/code-view {} :superficie)))
  (is (= :clojure (editor/code-view {:code-view :clojure} :superficie)))
  (is (= :superficie (editor/code-view {:code-view :superficie} :clojure))))
