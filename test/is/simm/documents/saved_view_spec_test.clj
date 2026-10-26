(ns is.simm.documents.saved-view-spec-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.documents.saved-view-spec :as spec]))

(def table
  {:version 1 :id #uuid "4207e77a-1a41-4ea4-92a8-5440e9d8d6a7"
   :source {:kind :pages :roots ["companies"]}
   :presentation :table
   :columns [{:kind :title} {:property #uuid "5f935639-3841-4861-bef0-53e8662cb18d"}]})

(defn- refused? [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e (= ::spec/refused (:type (ex-data e))))))

(deftest descriptors-are-inert-data-with-explicit-stable-identities
  (let [decoded (spec/decode (str "; Saved page view\n" (pr-str table)))]
    (is (= (assoc table :limit 50 :order [{:kind :title :direction :asc}]
                         :source {:kind :pages :roots ["companies"] :types []}) decoded))
    (is (= decoded (spec/decode (pr-str decoded)))))
  (let [page (random-uuid)
        form (spec/validate (-> table (assoc :presentation :form) (assoc-in [:source :page] page)))]
    (is (= 1 (:limit form)))
    (is (= page (get-in form [:source :page]))))
  (is (refused? #(spec/validate (assoc table :presentation :form))))
  (is (refused? #(spec/validate (-> table (assoc :presentation :form :limit 2)
                                      (assoc-in [:source :page] (random-uuid)))))))

(deftest imported-declarations-cannot-install-authority-or-executable-behavior
  (doseq [value [(assoc table :permissions [:write]) (assoc table :query '[:find ?e])
                 (assoc table :renderer '(println :executed))
                 (assoc-in table [:source :connection] "private")
                 (assoc-in table [:source :kind] :room-database)
                 (assoc-in table [:columns 1 :write] {:attribute :party/password-hash})
                 (assoc table :order [{:fn 'clojure.core/slurp}])
                 (assoc table :version 2)]]
    (is (refused? #(spec/validate value))))
  (doseq [source ["#=(System/exit 1)" "#evil {}" (str (pr-str table) " {}")
                  (str (pr-str table) " :is.simm.documents.saved-view-spec/eof {}")
                  "{:version 1 :version 1}" "(do (println :executed))"]]
    (is (refused? #(spec/decode source)))))

(deftest malformed-and-oversized-selections-never-widen-to-the-entire-memory
  (doseq [source [{:kind :pages :root "companies"} {:kind :pages :roots []}
                  {:kind :pages :roots ["../private"]} {:kind :pages :roots ["/companies"]}
                  {:kind :pages :roots ["companies/"]} {:kind :pages :roots [".git"]}
                  {:kind :pages :roots ["companies" "companies"]}
                  {:kind :pages :roots [""] :types ["not-a-uuid"]}
                  {:kind :pages :roots [""] :page "not-a-uuid"}]]
    (is (refused? #(spec/validate (assoc table :source source)))))
  (doseq [columns [[] ["uuid"] [{:kind :title} {:kind :title :label "Duplicate"}]
                   [{:property "uuid"}] [{:property (random-uuid) :label "\n"}]
                   (vec (repeat 17 {:kind :title}))]]
    (is (refused? #(spec/validate (assoc table :columns columns)))))
  (doseq [limit [0 -1 51 1.5 "50" nil]]
    (is (refused? #(spec/validate (assoc table :limit limit))))))

(deftest lexical-bounds-precede-reading-without-misreading-strings-and-comments
  (is (refused? #(spec/decode (apply str (repeat 9 "[")))))
  (is (refused? #(spec/decode (str (apply str (repeat 20000 "[")) "0"))))
  (is (refused? #(spec/decode "\\{")))
  (is (refused? #(spec/decode (str "\"" (char 0xD800) "\""))))
  (testing "literal collection characters and semicolons in labels are data"
    (let [value (assoc table :columns [{:kind :title :label "[Company] {name}; \"title\""}])]
      (is (= (:columns value) (:columns (spec/decode (pr-str value)))))
      (is (= (spec/validate value)
             (spec/decode (str "; [[[ collection-like comment\n" (pr-str value))))))))

(def scorecard-table
  {:version 1 :id #uuid "36784910-4e2b-4f10-92e9-46882a3dc8fa"
   :source {:kind :scorecard :room :current :scorecard #uuid "ea3449d4-d173-48ad-af24-9e4c0116ca4e"}
   :presentation :table
   :columns [{:kind :candidate} {:kind :pass-rate} {:kind :cost}]})

(deftest scorecard-declarations-select-one-immutable-readonly-room-snapshot
  (let [decoded (spec/decode (pr-str scorecard-table))]
    (is (= (assoc scorecard-table :limit 32) decoded))
    (is (not (contains? decoded :order)))
    (is (= decoded (spec/decode (pr-str decoded)))))
  (doseq [value [(assoc scorecard-table :presentation :form)
                 (assoc scorecard-table :order [{:kind :title :direction :asc}])
                 (assoc scorecard-table :limit 33)
                 (assoc-in scorecard-table [:source :room] (random-uuid))
                 (assoc-in scorecard-table [:source :scorecard] "not-a-uuid")
                 (assoc-in scorecard-table [:source :roots] [""])
                 (assoc-in scorecard-table [:source :types] [(random-uuid)])
                 (assoc-in scorecard-table [:columns 1] {:property (random-uuid)})
                 (assoc-in scorecard-table [:columns 1] {:kind :eval :code "(read-secret)"})
                 (assoc scorecard-table :columns [{:kind :candidate} {:kind :candidate}])
                 (assoc scorecard-table :permissions [:merge])]]
    (is (refused? #(spec/validate value)))))
