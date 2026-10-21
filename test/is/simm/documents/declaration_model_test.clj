(ns is.simm.documents.declaration-model-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.property-model :as model]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [is.simm.model.crud :as crud]
            [kontor.governance :as governance]))

(def options {:roots [""] :schema-revision :declaration-fixture-v1 :entity-profile space/profile})
(def page-id #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def person-id #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def block-id #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")
(def target {:kind :page})

(defn- write! [conn path text]
  (repo/write! conn path (markdown/source-bytes text)))

(defn- request [db target types additions]
  (let [preview (model/preview-declaration-in db "wiki/a.md" target
                                            {:types types :additions (mapv #(dissoc % :input) additions)})]
    (-> preview (dissoc :fields) (assoc :additions additions))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(defn- rejects [expected f]
  (is (= expected (reason f))))

(deftest trusted-types-and-required-values-are-one-read-only-source-preparation
  ;; Exclusive fixture connections only: no global runtime/context replacement.
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? false :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (space/provision! conn)
        (let [company (crud/create-type! conn {:name "DeclarationCompany"})
              research (crud/create-type! conn {:name "DeclarationResearch"})
              revenue (crud/create-morphism! conn {:name "S/DeclarationCompany/revenue"
                                                   :src "S/DeclarationCompany" :dst "S/Number" :optional? false})
              founder (crud/create-morphism! conn {:name "S/DeclarationCompany/founder"
                                                   :src "S/DeclarationCompany" :dst "S/Person"})
              active (crud/create-morphism! conn {:name "S/DeclarationResearch/active"
                                                  :src "S/DeclarationResearch" :dst "S/Boolean"})
              person (:entity/uuid (d/pull @conn [:entity/uuid] [:entity/name "S/Person"]))
              original (str "﻿---\r\nsimmis-id: " page-id " # keep identity\r\nunknown: {keep: '😀'}\r\n---\r\n# Company\r\n\r\n"
                            "<!-- simmis:block " block-id " -->\r\nResearch\r\n<!-- /simmis:block -->\r\n")
              revenue-input {:alias "revenue" :property revenue :declaring-type company :input {:text "0"}}
              founder-input {:alias "founder" :property founder :declaring-type company :input {:text (str person-id)}}
              active-input {:alias "active" :property active :declaring-type research :input {:text "false"}}]
          (write! conn "wiki/a.md" original)
          (write! conn "wiki/person.md"
                  (str "---\nsimmis-id: " person-id "\nsimmis: {version: 1, types: [\"" person "\"]}\n---\n# Person\n"))
          (let [db @conn req (request db target [company research] [revenue-input founder-input active-input])
                prepared (model/prepare-declaration-in db options req)
                parsed (markdown/parse (:text prepared))
                block-target {:kind :block :id block-id}
                block-request (request db block-target [research] [active-input])
                block-prepared (model/prepare-declaration-in db options block-request)]
            (testing "one immutable basis, no native write, lossless unrelated source"
              (is (= db @conn))
              (is (= #{:path :expected :text} (set (keys prepared))))
              (is (= (subs original (:body-from (markdown/parse original)))
                     (subs (:text prepared) (:body-from parsed))))
              (is (str/includes? (:text prepared) "unknown: {keep: '😀'}\r\n"))
              (is (str/includes? (:text prepared) (str "simmis-id: " page-id " # keep identity\r\n")))
              (is (= 0 (get-in parsed [:frontmatter :properties "revenue"])))
              (is (false? (get-in parsed [:frontmatter :properties "active"])))
              (is (= (str person-id) (get-in parsed [:frontmatter :properties "founder"])))
              (is (= :valid (:status (view/validate-replacement db options
                                                              {:path "wiki/a.md" :expected (:expected req)
                                                               :source (:text prepared)})))))
            (testing "explicit block scope and distinct structural role"
              (is (false? (get-in (markdown/parse (:text block-prepared))
                                 [:frontmatter :properties "simmis" "blocks" (str block-id) "values" "active"])))
              (rejects :incompatible-structural-role
                       #(request db block-target [(:id (first (:types (model/inspect-in db "wiki/a.md" target))))] [])))
            (testing "required values, duplicate aliases, exact relation UUIDs"
              (rejects :invalid-selection #(model/prepare-declaration-in db options (request db target [company] [])))
              (rejects :invalid-declaration-edit #(request db target [company] [revenue-input revenue-input]))
              (rejects :invalid-selection
                       #(model/prepare-declaration-in db options
                                                      (request db target [company]
                                                               [revenue-input (assoc-in founder-input [:input :text] (str page-id))]))))
            (testing "old AND newly selected definitions must match preview"
              (rejects :stale-property-source-or-schema
                       #(model/prepare-declaration-in db options (assoc req :scope-revision (random-uuid))))
              (rejects :stale-proposed-definitions
                       #(model/prepare-declaration-in (d/db-with db [{:entity/uuid revenue :morphism/optional? true}]) options req))
              (rejects :invalid-declaration-edit #(model/prepare-declaration-in db options (assoc req :schema {}))))
            (testing "a no-op does not add empty reserved metadata"
              (is (= original (:text (model/prepare-declaration-in db options (request db target [] []))))))
            (testing "no source, definition or capability payload leaks through preview"
              (let [p (model/preview-declaration-in db "wiki/a.md" target
                                                    {:types [company] :additions [(dissoc revenue-input :input)]})
                    field (first (filter #(= revenue (:id %)) (:fields p)))
                    unbound (first (filter #(= founder (:id %)) (:fields p)))]
                (is (not-any? #(contains? p %) [:db :source :compiled :authority :schema]))
                (is (every? #(not-any? (set (keys %)) [:morphism :computation :validation :schema :compiled]) (:fields p)))
                (is (:new? field))
                (is (:editable? field))
                (is (nil? (:reason field)))
                (is (:required? field))
                (is (:needs-binding? unbound))
                (is (false? (:editable? unbound)))))
            (write! conn "wiki/a.md" (:text prepared))
            (testing "removal cannot orphan existing bindings or rebind unknown values"
              (is (some? (reason #(request @conn target [] []))))
              (rejects :existing-binding-or-value
                       #(request @conn target [company research] [(assoc active-input :alias "unknown")])))
            (testing "source changed since preview"
              (rejects :stale-property-source-or-schema #(model/prepare-declaration-in @conn options req)))))
        (finally
          (governance/ungovern! conn)
          (d/release conn true)
          (d/delete-database cfg))))))
