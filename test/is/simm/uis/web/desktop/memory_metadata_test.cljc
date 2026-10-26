(ns is.simm.uis.web.desktop.memory-metadata-test
  (:require [is.simm.uis.web.desktop.memory-metadata :as metadata]
            [is.simm.documents.attachments :as attachments]
            [is.simm.uis.web.desktop.memory-metadata-view :as view]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(defn- request [paths] {:folders ["" "wiki"] :paths (vec paths)})

(deftest metadata-observations-retire-stale-page-view-requests-without-dropping-prior-display
  (let [stamp (random-uuid) id (random-uuid) slot [:page-view "views/report.md" id] token (random-uuid)
        state {:target-stamp stamp :loading {slot true} :requests {slot token}
               :request-contexts {slot {:scope :target}} :page-views {["views/report.md" id] {:reply :prior}}}
        reply {:protocol-version 2 :target-stamp stamp :folders {} :entries {}}
        result (view/consume state reply {:connection :socket})]
    (is (not (get-in result [:loading slot])))
    (is (nil? (get-in result [:requests slot])))
    (is (false? (editor/response-current? result slot token)))
    (is (= :prior (get-in result [:page-views ["views/report.md" id] :reply])))
    (is (= 1 (:read-epoch result)))))

(deftest identical-and-subset-owners-share-a-whole-request
  (let [same (request ["wiki/a.md"])
        groups (metadata/batches {:sidebar same :first-tab same
                                  :second-tab {:folders [""] :paths []}})]
    (is (= 1 (count groups)))
    (is (= same (:request (first groups))))
    (is (= #{:sidebar :first-tab :second-tab} (:owners (first groups))))))

(deftest aggregate-overflow-keeps-each-owner-whole-without-dropping-paths
  (let [a (mapv #(str "wiki/a" % ".md") (range 40))
        b (mapv #(str "wiki/b" % ".md") (range 40))
        demands {:first (request a) :second (request b)}
        groups (metadata/batches demands)]
    (is (= 2 (count groups)))
    (is (= #{:first :second} (into #{} (mapcat :owners) groups)))
    (is (= (set (concat a b)) (into #{} (mapcat #(get-in % [:request :paths])) groups)))
    (doseq [{:keys [owners request]} groups]
      (is (<= (count (:paths request)) 64))
      (doseq [owner owners]
        (is (= (set (get-in demands [owner :paths])) (set (:paths request))))))))

(deftest exact-boundary-and-request-order-are-deterministic
  (let [paths (mapv #(str "wiki/" % ".md") (range 64))
        a (request (take 63 paths)) b (request [(last paths)])
        groups (metadata/batches (array-map :b b :a a))]
    (is (= 1 (count groups)))
    (is (= 64 (count (get-in groups [0 :request :paths]))))
    (is (= groups (metadata/batches (array-map :a a :b b))))
    (is (= [] (metadata/batches {})))
    (is (= {:folders ["" "wiki"] :paths ["wiki/a.md"]}
           (metadata/normalize-request {:folders ["wiki" "" "wiki"]
                                        :paths ["wiki/a.md" "wiki/a.md"]})))))

(deftest explicit-absence-is-covered-and-omitted-paths-or-folders-are-not
  (let [snapshot {:folders {"" {:entries []}} :entries {"wiki/missing.md" nil}}]
    (is (metadata/covered? snapshot {:folders [""] :paths ["wiki/missing.md"]}))
    (is (not (metadata/covered? snapshot {:folders [""] :paths ["wiki/unknown.md"]})))
    (is (not (metadata/covered? snapshot {:folders ["wiki"] :paths ["wiki/missing.md"]})))
    (is (not (metadata/covered? {} {:folders [] :paths []})))))

(deftest invalid-owner-demands-refuse-instead-of-silently-truncating
  (doseq [bad [nil {:folders [] :paths [] :extra true}
               {:folders (vec (repeat 65 "wiki")) :paths []}
               {:folders [] :paths (mapv str (range 65))}
               {:folders [] :paths [nil]} {:folders '() :paths []}]]
    (is (try (metadata/batches {:good (request []) :bad bad}) false
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
               (= :is.simm.uis.web.desktop.memory-metadata/invalid-demand (:type (ex-data e))))))))

(deftest one-observation-reconciles-each-views-dispatch-generation
  (let [stamp (random-uuid) path "wiki/a.md"
        draft (assoc (editor/draft {:path path :text "saved" :kind :text
                                   :target-stamp stamp :entry {:content :old}})
                     :generation 1)
        state {:target-stamp stamp :drafts {path draft}}
        reply {:protocol-version 2 :target-stamp stamp :entries {path {:content :new}}
               :folders {} :permissions {:write? true}}
        frame {:connection :socket :generations {path 1}}
        first-view (view/consume state reply frame)
        edited-view (view/consume (assoc-in state [:drafts path :generation] 2) reply frame)]
    (is (= {:content :new} (get-in first-view [:drafts path :incoming-entry])))
    (is (nil? (get-in edited-view [:drafts path :incoming-entry])))
    (is (= "saved" (get-in first-view [:drafts path :text])))
    (is (= "saved" (get-in edited-view [:drafts path :text])))
    (is (= :socket (:verified-connection first-view)))))

(deftest superseded-stream-cannot-refill-closed-folders-or-lose-current-intents
  (let [stamp (random-uuid)
        current {:target-stamp stamp :stream-marker :new
                 :file-intents {"new.md" true "missing.md" true}
                 :loading {[:file "new.md"] true}}
        reply {:protocol-version 2 :target-stamp stamp :folders {"closed" {:entries [:old]}}
               :entries {"missing.md" nil}}
        result (view/consume current reply {:stream-marker :old})]
    (is (nil? (get-in result [:folders "closed"])))
    (is (= {"new.md" true} (:file-intents result)))
    (is (= {[:file "new.md"] true} (:metadata-followups result)))
    (is (false? (get-in result [:loading [:file "new.md"]])))
    (is (string? (get-in result [:errors [:file "missing.md"]])))))

(deftest target-change-retires-target-reads-but-keeps-evaluation-receipts
  (let [stamp (random-uuid) next-stamp (random-uuid) receipt {:success true}
        state {:target-stamp stamp :upload-intent {:path "a.md"}
               :request-contexts {[:cell "a.md" "x"] {:scope :target}
                                  [:file "a.md"] {:scope :target}}
               :accepted {[:cell "a.md" "x"] :receipt [:file "a.md"] :old}
               :evaluations {"a.md" receipt} :evaluation-history [receipt]}
        result (view/consume state {:protocol-version 2 :target-stamp next-stamp
                                    :entries {} :folders {}} {})]
    (is (= next-stamp (:target-stamp result)))
    (is (= {:path "a.md"} (:cancelled-upload result)))
    (is (= {[:cell "a.md" "x"] :receipt} (:accepted result)))
    (is (= [receipt] (:evaluation-history result)))
    (is (nil? (:evaluations result)) "Current-file results retire; historical receipts remain")
    (is (nil? (:upload-intent result)))))

(deftest followups-use-current-intents-and-distinguish-absence-from-omission
  (let [state {:file-intents {"new.md" true "gone.md" true}
               :folder-intents {"new-folder" true}
               :metadata-followups {[:office "report.docx"] true}
               :drafts {"report.docx" {:office {:part "word/document.xml"}}}
               :history {"old.md" {}} :query "current"}
        plan (view/followups state {:target-stamp (random-uuid)
                                    :entries {"gone.md" nil}})]
    (is (= #{"new.md"} (:files plan)))
    (is (= #{"new-folder"} (:folders plan)))
    (is (= [{:path "report.docx" :part "word/document.xml" :reading? false}] (:office plan)))
    (is (= ["old.md"] (:histories plan)))
    (is (:search? plan))))

(deftest planning-captures-typing-without-making-it-a-fetch-generation
  (let [state {:selected "a.md" :stream-marker :current
               :drafts {"a.md" {:generation 1}}}
        original (first (metadata/plans {:tab (view/admission :socket state)} 0))
        typed (first (metadata/plans {:tab (view/admission :socket (assoc-in state [:drafts "a.md" :generation] 2))} 0))
        invalidated (first (metadata/plans {:tab (view/admission :socket state)} 1))]
    (is (= (:generation original) (:generation typed)))
    (is (= 1 (get-in original [:owners :tab :generations "a.md"])))
    (is (= 2 (get-in typed [:owners :tab :generations "a.md"])))
    (is (not= (:generation original) (:generation invalidated)))))

(deftest batch-envelopes-retain-independent-epochs-coverage-and-publications
  (let [owners (into {} (for [owner [:a :b]
                             :let [paths (mapv #(str (name owner) % ".md") (range 40))]]
                         [owner (view/admission :socket {:file-intents (zipmap paths (repeat true))})]))
        [a b :as plans] (metadata/plans owners :hint)
        epoch [:socket :world]
        a-reply (assoc a :epoch epoch :sequence 1)
        b-reply (assoc b :epoch epoch :sequence 2)
        state {:envelopes {(:request a) a-reply (:request b) b-reply}}]
    (is (= 2 (count plans)))
    (is (= a (metadata/next-plan {} plans epoch)))
    (is (= b (metadata/next-plan {:envelopes {(:request a) a-reply}} plans epoch)))
    (is (nil? (metadata/next-plan state plans epoch)))
    (is (= 1 (:sequence (metadata/observation-for state plans epoch :a))))
    (is (= 2 (:sequence (metadata/observation-for state plans epoch :b))))
    (is (nil? (metadata/observation-for state plans [:socket :fork] :a)))
    (is (nil? (metadata/observation-for state (metadata/plans owners :new-hint) epoch :a)))
    (is (nil? (metadata/observation-for state plans epoch :closed-owner)))))

(deftest image-dependencies-use-metadata-without-implicit-file-source-loads
  (let [stamp (random-uuid) id (random-uuid)
        analysis {:nodes [{:kind :Paragraph :children [{:kind :Image :url "images/frame.png"}]}]}
        state {:selected "captures/source.md" :target-stamp stamp
               :drafts {"captures/source.md" {:analysis analysis :generation 1}}}
        image "captures/images/frame.png" entry {:content id :size 12 :mode 33188}
        reply {:protocol-version 2 :target-stamp stamp :folders {} :entries {image entry}}
        observed (view/consume state reply {:connection :socket :generations {"captures/source.md" 1}})
        urls #(attachments/urls (str id) "captures/source.md" analysis (:observed-entries %) (:target-stamp %))]
    (is (= [image "captures/source.md"] (:paths (view/request state))))
    (is (empty? (:files (view/followups observed reply))))
    (is (= 1 (count (urls observed))))
    (let [replacement (view/consume observed (assoc-in reply [:entries image :content] (random-uuid)) {})
          deleted (view/consume replacement (assoc-in reply [:entries image] nil) {})]
      (is (not= (urls observed) (urls replacement)))
      (is (empty? (urls deleted)))
      (is (nil? (get-in deleted [:errors [:file image]]))))
    (is (empty? (urls (editor/observe-target observed (random-uuid)))))))

(deftest image-dependency-budget-does-not-overflow-an-existing-owner-demand
  (let [paths (mapv #(str "existing" % ".md") (range 63))
        selected (first paths)
        state {:selected selected :drafts (zipmap paths (repeat {}))}
        state (assoc-in state [:drafts selected :analysis]
                        {:nodes (mapv #(hash-map :kind :Image :url (str "image" % ".png")) (range 100))})
        demand (view/request state)]
    (is (= 64 (count (:paths demand))))
    (is (every? (set (:paths demand)) paths))
    (is (= demand (metadata/normalize-request demand)))))
