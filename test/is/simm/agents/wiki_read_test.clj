(ns is.simm.agents.wiki-read-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.runtime.ctx :as runtime]
            [dvergr.search.secondary :as secondary]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.agents.wiki-read :as wiki]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [sci.core :as sci]))

(defn- snapshot [title]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn
                  [{:db/ident :S.Page/title :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                   {:db/ident :block/content :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                   {:db/ident :block/order :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                   {:db/ident :block/parent :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                   {:db/ident :block/references :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}])
      (d/transact conn [{:db/id "a" :S.Page/title title} {:db/id "b" :S.Page/title "Beta"}
                        {:db/id "c" :S.Page/title "Citing"}
                        {:block/parent "a" :block/content "<p>Second &quot;quote&quot;&nbsp;here</p>" :block/order "b"}
                        {:block/parent "a" :block/content "<p>First</p>" :block/order "a" :block/references ["b"]}
                        {:block/parent "c" :block/content "Citation" :block/order "a" :block/references ["a"]}])
      @conn)))

(defn- operations [bindings]
  [[(get bindings 'pages) []]
   [(get bindings 'read-page) ["Alpha"]]
   [(get bindings 'search) ["Alpha"]]
   [(get bindings 'backlinks) ["Alpha"]]
   [(get bindings 'neighborhood) ["Alpha"]]])

(deftest each-read-pins-one-resolver-result-and-preserves-page-and-link-projections
  (let [db (snapshot "Alpha") calls (atom 0)
        bindings (wiki/bindings #(do (swap! calls inc) [{:name "Handbook" :db db}]))]
    (doseq [[f args] (operations bindings)]
      (reset! calls 0)
      (apply f args)
      (is (= 1 @calls)))
    (is (= [{:kb "Handbook" :pages ["Alpha" "Beta" "Citing"]}] ((get bindings 'pages))))
    (is (= {:kb "Handbook" :title "Alpha" :blocks ["First" "Second \"quote\" here"]}
           ((get bindings 'read-page) "Alpha")))
    (is (nil? ((get bindings 'read-page) nil)))
    (is (nil? ((get bindings 'read-page) "Missing")))
    (is (= [{:kb "Handbook" :title "Citing"}] ((get bindings 'backlinks) "Alpha")))
    (is (= {:page "Alpha" :links-to [{:kb "Handbook" :title "Beta"}]
            :linked-from [{:kb "Handbook" :title "Citing"}]}
           ((get bindings 'neighborhood) "Alpha")))
    (is (= #{'pages 'read-page 'search 'backlinks 'neighborhood} (set (keys bindings))))))

(deftest search-keeps-ranked-fulltext-snippets-limit-and-regex-fallback
  (let [db (snapshot "Alpha") calls (atom 0)
        search (get (wiki/bindings #(do (swap! calls inc) [{:name "Handbook" :db db}])) 'search)
        eid (d/q '[:find ?e . :where [?e :block/content "<p>First</p>"]] db)]
    (with-redefs [secondary/search
                  (fn [actual ident query opts]
                    (is (identical? db actual))
                    (is (= :kb/wiki-fulltext ident))
                    (is (= "[" query))
                    (is (= {:limit 12} opts))
                    (cons [999999 100] (mapv #(vector eid %) (range 1 16))))]
      (let [results (search "[")]
        (is (= 12 (count results)))
        (is (= (vec (range 15 3 -1)) (mapv :score results)))
        (is (every? #(= {:kb "Handbook" :title "Alpha" :score (:score %) :snippet "First"} %) results))
        (is (= 1 @calls))))
    (with-redefs [secondary/search (fn [& _] [])]
      (is (= [{:kb "Handbook" :title "Alpha"}] (search "ALP.*")))
      (is (thrown? java.util.regex.PatternSyntaxException (search "["))))
    (let [resolve clojure.core/requiring-resolve]
      (with-redefs [clojure.core/requiring-resolve
                    (fn [symbol] (when-not (= 'dvergr.search.secondary/search symbol) (resolve symbol)))]
        (is (= [{:kb "Handbook" :title "Alpha"}] (search "ALP.*")))))
    (with-redefs [secondary/search (fn [& _] (throw (ex-info "Broken index" {:type ::broken-index})))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Broken index" (search "Alpha"))))))

(deftest separate-kbs-preserve-duplicate-titles-and-cross-target-search-ranking
  (let [first-db (snapshot "Alpha") second-db (snapshot "Alpha")
        bindings (wiki/bindings (constantly [{:name "First" :db first-db} {:name "Second" :db second-db}]))
        first-eid (d/q '[:find ?e . :where [?e :S.Page/title "Alpha"]] first-db)
        second-eid (d/q '[:find ?e . :where [?e :S.Page/title "Alpha"]] second-db)]
    (is (= ["First" "Second"] (mapv :kb ((get bindings 'pages)))))
    (is (= "First" (:kb ((get bindings 'read-page) "Alpha"))))
    (with-redefs [secondary/search (fn [db & _]
                                     (if (identical? first-db db) [[first-eid 1]] [[second-eid 5]]))]
      (is (= [{:kb "Second" :title "Alpha" :score 5 :snippet nil}
              {:kb "First" :title "Alpha" :score 1 :snippet nil}]
             ((get bindings 'search) "Alpha"))))))

(deftest ordinary-participant-adapter-pins-each-connection-once-and-keeps-vocabulary-metadata
  (let [db (snapshot "Alpha") reads (atom 0)
        conn (reify clojure.lang.IDeref (deref [_] (swap! reads inc) db))
        interpreter (sci/init {})]
    ;; The private two-arity installer is the trusted unbound adapter; real
    ;; participants supply identities, covered by the ReBAC integration test.
    (#'room-agents/add-wiki-ns! interpreter [{:name "Handbook" :conn conn}])
    (doseq [source ["(wiki/pages)" "(wiki/read-page \"Alpha\")" "(wiki/search \"Alpha\")"
                    "(wiki/backlinks \"Alpha\")" "(wiki/neighborhood \"Alpha\")"]]
      (reset! reads 0)
      (sci/eval-string* interpreter source)
      (is (= 1 @reads) source))
    (is (= '([]) (sci/eval-string* interpreter "(:arglists (meta (var wiki/pages)))")))
    (is (string? (sci/eval-string* interpreter "(:doc (meta (var wiki/pages)))")))
    (is (= {:error "summarize! needs a room-bound wiki ns"}
           (sci/eval-string* interpreter "(wiki/summarize!)")))))

(deftest missing-or-refused-targets-cannot-look-like-an-authorized-empty-wiki
  (doseq [targets [nil {} '() [{:name "Missing" :db nil}]]
          [f args] (operations (wiki/bindings (constantly targets)))]
    (is (thrown? clojure.lang.ExceptionInfo (apply f args))))
  (doseq [[f args] (operations (wiki/bindings #(throw (ex-info "Read revoked" {:type ::revoked}))))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Read revoked" (apply f args))))
  (let [empty (wiki/bindings (constantly []))]
    (is (= [] ((get empty 'pages))))
    (is (nil? ((get empty 'read-page) "Alpha")))
    (is (= {:page "Alpha" :links-to [] :linked-from []} ((get empty 'neighborhood) "Alpha")))))

(deftest saved-sci-functions-resolve-root-child-and-grandchild-snapshots-on-use
  (let [owner (context/create-execution-context)
        root-db (snapshot "Root") child-db (snapshot "Child") grand-db (snapshot "Grandchild")
        resolver (fn []
                   (binding [ec/*execution-context* (runtime/selected-context owner)]
                     (or (ec/get-state [:wiki-test/targets])
                         (throw (ex-info "No selected work-world KB" {:type ::missing-world})))))
        interpreter (sci/init {:namespaces {'wiki (wiki/bindings resolver)}})
        pages #(sci/eval-string* interpreter "(saved-pages)")]
    (binding [ec/*execution-context* owner]
      (ec/swap-state! [:wiki-test/targets] (constantly [{:name "Root" :db root-db}])))
    (sci/eval-string* interpreter "(def saved-pages wiki/pages)")
    (let [child (context/fork-context owner)]
      (binding [ec/*execution-context* child]
        (ec/swap-state! [:wiki-test/targets] (constantly [{:name "Child" :db child-db}])))
      (let [grandchild (context/fork-context child)]
        (binding [ec/*execution-context* grandchild]
          (ec/swap-state! [:wiki-test/targets] (constantly [{:name "Grandchild" :db grand-db}]))
          (is (= [{:kb "Grandchild" :pages ["Beta" "Citing" "Grandchild"]}] (pages))))
        (binding [ec/*execution-context* child]
          (is (= [{:kb "Child" :pages ["Beta" "Child" "Citing"]}] (pages)))
          (ec/swap-state! [:wiki-test/targets] (constantly nil))
          (is (thrown? Exception (pages))))
        (binding [ec/*execution-context* owner]
          (is (= [{:kb "Root" :pages ["Beta" "Citing" "Root"]}] (pages))))))))
