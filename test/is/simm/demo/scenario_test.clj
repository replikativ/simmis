(ns is.simm.demo.scenario-test
  "Seeded history has to land WHERE the scenario says it did and say WHO wrote
   it, because the Timelines rail is derived from `:db/txInstant` and the audit
   panel from `:tx/author` — both read off the transaction, neither off the row.

   The bug this pins: seeding wrote a whole company inside one minute, so 148 of
   the rail's 149 dots landed inside its leftmost 5%. An assertion that the
   content transactions carry exactly the scenario's instants is the same claim
   stated in a form that fails."
  (:require [clojure.set]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.demo.scenario :as scenario]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.store :as store]
            [is.simm.runtimes.context :as ctx]))

(def ^:private add-page! #'scenario/add-page!)
(def ^:private link-page! #'scenario/link-page!)
(def ^:private chronological #'scenario/chronological)
(def ^:private load-book! #'scenario/load-book!)

(defn- fresh-kb
  "A KB store with the real schema and seed in it. `store/install!`, not
   `full-schema` alone: `:S.Page/title` is generated from the seed's morphisms
   and `:instance/of-role [:entity/name \"S/Page\"]` needs the role entity to
   exist, so a bare schema transaction leaves pages untransactable."
  []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :keep-history? true
             :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (store/install! conn)
      conn)))

(def ^:private content-tx-q
  "The rail's own query (`timeline-source/content-tx-q`), so this test measures
   what the view measures rather than an approximation of it."
  '[:find ?tx ?inst
    :where
    (or [_ :block/content _ ?tx]
        [_ :S.Page/title _ ?tx])
    [?tx :db/txInstant ?inst]])

(defn- content-txs [db]
  (->> (d/q content-tx-q db)
       (mapv (fn [[tx inst]]
               {:tx tx :inst inst
                :author (d/q '[:find ?a . :in $ ?tx :where [?tx :tx/author ?a]] db tx)}))))

(defn- titles [db] (set (d/q '[:find [?t ...] :where [_ :S.Page/title ?t]] db)))

;; `store/install!` seeds wiki pages of its own — "Getting Started", "SKILL" —
;; so a store is never content-transaction-empty, and every measurement here is
;; against what the scenario ADDED. That boot content is also the reason the
;; rail's own query says what it says about installation noise: the install runs
;; at wall clock and there is no :tx-meta to pass it.
(defn- since-baseline [baseline db]
  (remove #(contains? baseline (:tx %)) (content-txs db)))

(defn- ->inst [s] (java.util.Date/from (java.time.Instant/parse s)))

(deftest a-broken-book-entry-fails-the-scenario
  (testing "the manifest cannot report entries that Kontor rejected"
    (ctx/with-server-context
      (let [conn (fresh-kb)
            scope (random-uuid)
            spec {:room "Operations"
                  :commodity {:symbol "USD" :name "US Dollar" :precision 2}
                  :entries [{:at "2026-03-02T09:00:00Z"
                             :narration "Invalid opening entry"
                             :debit "Assets:Missing"
                             :credit "Equity:Opening"
                             :amount 10
                             :journal-type :general}]}]
        (with-redefs [room-dbs/connect-room-database (constantly conn)]
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo #"Scenario book entry failed"
               (load-book! [{:room/name "Operations"
                             :room/content-db-scope scope}]
                           spec))))
        (is (zero? (or (d/q '[:find (count ?posting) .
                              :where [?posting :kontor.posting/amount _]]
                            @conn)
                       0)))))))

(deftest seeded-pages-land-at-their-narrative-instant
  (testing "every content transaction carries the scenario's instant, not the seed run's"
    (ctx/with-server-context
      (let [conn (fresh-kb)
            baseline (set (map :tx (content-txs @conn)))
            author (random-uuid)
            pages [{:title "Handbook" :at "2026-03-02T09:00:00Z"
                    :blocks ["<p>the wedge is maintenance triage</p>"]}
                   {:title "Escalation Policy" :at "2026-03-05T09:00:00Z"
                    :blocks ["<p>escalate early and cheaply</p>"]}]]
        (doseq [p pages] (add-page! conn "KB" author p))
        (let [txs (since-baseline baseline @conn)]
          (is (= #{(->inst "2026-03-02T09:00:00Z") (->inst "2026-03-05T09:00:00Z")}
                 (set (map :inst txs)))
              "a wall-clock instant here is a dot in the smudge at the right edge")
          (is (= #{(str author)} (set (map :author txs)))
              "an unattributed transaction leaves the audit panel unable to name anyone"))))))

(deftest forward-wikilinks-do-not-mint-stub-pages
  (testing "linking after every page exists reuses the real page"
    (ctx/with-server-context
      (let [conn (fresh-kb)
            baseline (set (map :tx (content-txs @conn)))
            seeded-titles (titles @conn)
            author (random-uuid)
            ;; The scenario links forward constantly — the first Handbook page
            ;; points at four pages seeded after it. Linking as each page landed
            ;; minted a stub for the target; `:entity/name` being unique identity
            ;; then merged the real page onto it, so what survived was the stub's
            ;; placeholder empty block above the real content, and a wall-clock
            ;; transaction from inside `link-block-references!`.
            pages [{:title "Handbook" :at "2026-03-02T09:00:00Z"
                    :blocks ["<p>see [[Escalation Policy]]</p>"]}
                   {:title "Escalation Policy" :at "2026-03-05T09:00:00Z"
                    :blocks ["<p>escalate early and cheaply</p>"]}]]
        (doseq [p pages] (add-page! conn "KB" author p))
        (doseq [p pages] (link-page! conn "KB" p))
        (is (= #{"Escalation Policy" "Handbook"}
               (clojure.set/difference (titles @conn) seeded-titles))
            "a third title means the forward link minted a page of its own")
        (is (= ["<p>escalate early and cheaply</p>"]
               (d/q '[:find [?c ...] :where
                      [?p :S.Page/title "Escalation Policy"]
                      [?b :block/parent ?p] [?b :block/content ?c]]
                    @conn))
            "an empty block here is the stub's placeholder, merged onto the page")
        (is (= #{(->inst "2026-03-02T09:00:00Z") (->inst "2026-03-05T09:00:00Z")}
               (set (map :inst (since-baseline baseline @conn))))
            "a stub page would also be a content transaction stamped now")
        (is (some? (d/q '[:find ?target .
                          :where [?b :block/references ?target]]
                        @conn))
            "the link still has to resolve — backlinks are queries over these")))))

(deftest scenario-items-seed-in-narrative-order
  (testing "file order does not decide transaction order"
    ;; Datahike accepts an out-of-order backdate silently — measured in a scratch
    ;; db, a transaction stamped two months before the one written ahead of it
    ;; raises nothing. What it costs is `d/since` at a cut between them returning
    ;; nothing at all, so the ordering has to be established before the write.
    (is (= ["2026-03-02T09:00:00Z" "2026-03-05T09:00:00Z" "2026-04-01T09:00:00Z"]
           (mapv :at (chronological [{:at "2026-04-01T09:00:00Z"}
                                     {:at "2026-03-05T09:00:00Z"}
                                     {:at "2026-03-02T09:00:00Z"}]))))))
