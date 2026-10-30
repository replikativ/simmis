(ns is.simm.model.github-proposals-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.model.github-integrations-test :as fixture]
            [is.simm.model.github-proposals :as git]))

(defn- body []
  (let [connection (#'fixture/connection)
        target (:target connection)
        basis (merge (select-keys target [:repository :store :backend :branch :ref])
                     {:configuration (random-uuid) :commit (random-uuid) :max-tx 1 :datoms 1})
        base (random-uuid) local (random-uuid) incoming (random-uuid) candidate (random-uuid)
        identity (git/incoming-identity connection
                                        {:head base :git-oid (apply str (repeat 40 "a")) :proof-id (random-uuid)}
                                        {:head incoming :git-oid (apply str (repeat 40 "b"))})
        root (fn [head] {:id (random-uuid) :head head :basis (assoc basis :commit (random-uuid))})]
    {:version 1 :identity identity :key (git/incoming-key identity)
     :producer {:sync-id (random-uuid) :claim (random-uuid) :by (random-uuid)}
     :proposal {:id (random-uuid) :scope (:registry-system target)
                :review-ref (str "refs/heads/github-review-" (random-uuid)) :base local :head candidate}
     :canonical-basis basis
     :roots {:base (root base) :local {:id (random-uuid) :head local :basis basis}
             :incoming (root incoming) :candidate (root candidate)}}))

(defn- proposal [body]
  (let [p (:proposal body) by (get-in body [:producer :by])]
    {:proposal/id (:id p) :proposal/room (get-in body [:identity :connection :room])
     :proposal/author by :proposal/github-key (:key body)
     :proposal/github-provenance (random-uuid) :proposal/github-provenance-id (git/provenance-id body)
     :proposal/forks [{:proposal.fork/scope (:scope p)
                       :proposal.fork/authority-scope (get-in body [:identity :connection :room])
                       :proposal.fork/branch (subs (:review-ref p) 11)
                       :proposal.fork/system-type :repo :proposal.fork/author by
                       :proposal.fork/base-commit (str (:base p))
                       :proposal.fork/head-commit (str (:head p))}]}))

(deftest incoming-identity-is-separate-from-local-base-and-merged-candidate
  (let [value (body) p (proposal value) replacement (random-uuid)
        revised (-> value (assoc-in [:proposal :head] replacement)
                    (assoc-in [:roots :candidate :head] replacement))]
    (is (= value (git/validate-provenance! value)))
    (is (= p (git/validate-proposal! value p)))
    (is (not= (get-in value [:identity :cursor :head]) (get-in value [:proposal :base])))
    (is (not= (get-in value [:identity :incoming :head]) (get-in value [:proposal :head])))
    (is (= (:key value) (git/incoming-key (:identity revised))))
    (is (not= (git/provenance-id value) (git/provenance-id revised)))
    (is (thrown? clojure.lang.ExceptionInfo (git/validate-proposal! revised p)))))

(deftest every-retained-role-and-physical-basis-must-correspond
  (let [value (body)]
    (doseq [invalid [(assoc-in value [:roots :base :head] (random-uuid))
                     (assoc-in value [:roots :incoming :head] (random-uuid))
                     (assoc-in value [:roots :local :head] (random-uuid))
                     (assoc-in value [:roots :candidate :head] (random-uuid))
                     (assoc-in value [:roots :incoming :basis :store] (random-uuid))
                     (assoc-in value [:roots :local :basis :commit] (random-uuid))
                     (assoc-in value [:roots :candidate :id] (get-in value [:roots :incoming :id]))
                     (assoc value :extra true)]]
      (is (thrown? clojure.lang.ExceptionInfo (git/validate-provenance! invalid))))))

(deftest a-generic-filing-cannot-substitute-its-head-author-or-settlement-kind
  (let [value (body) p (proposal value)]
    (doseq [invalid [(assoc-in p [:proposal/forks 0 :proposal.fork/head-commit] (str (random-uuid)))
                     (assoc p :proposal/author (random-uuid))
                     (assoc p :proposal/review (random-uuid))
                     (assoc p :proposal/adoption (random-uuid))]]
      (is (thrown? clojure.lang.ExceptionInfo (git/validate-proposal! value invalid))))))
