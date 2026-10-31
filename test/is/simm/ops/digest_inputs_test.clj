(ns is.simm.ops.digest-inputs-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.digest-inputs :as inputs]
            [is.simm.starter.conversation-digest :as starter]
            [is.simm.starter.conversation-digest-test :as fixture]))

(deftest digest-opt-in-requires-exact-frozen-originals-before-admission
  (let [{:keys [artifact selection]} (fixture/fixture)
        source (starter/source) params {"evidence-manifest" (pr-str selection)}
        originals {(str "/" (:path artifact)) (:text artifact)}]
    (is (= selection (inputs/validate-inputs! source params originals)))
    (is (nil? (inputs/validate-inputs! {} {} {})))
    (doseq [bad [{} (update originals (str "/" (:path artifact)) str " drift")]]
      (is (thrown? Exception (inputs/validate-inputs! source params bad))))
    (is (thrown? Exception (inputs/validate-inputs! source {} originals)))
    (is (thrown? Exception (inputs/validate-inputs! source {"evidence-manifest" "{} {}"} originals)))
    (is (thrown? Exception (inputs/validate-inputs! (assoc source "digest-policy.edn" "{:version 2}") params originals)))))
