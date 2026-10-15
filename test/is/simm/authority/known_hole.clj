(ns is.simm.authority.known-hole
  "Expected failures of the authority suite, made explicit.

   An attack that SUCCEEDS today must not turn CI red (the fixes come in later
   PRs), and must not be skipped either (a skipped attack is indistinguishable
   from a refuted one). `known-hole` runs the attack every time and inverts
   the assertion: it PASSES while the attack still works and FAILS the moment
   it stops working, with a message telling whoever fixed it to flip the test
   into an ordinary refusal assertion. Each fix PR is therefore forced to touch
   the test that documents its hole.

   Every hole carries the id it has in the P1 plan (`§2.7 …`, `§2.9.2 …`), or
   `UNLISTED …` for holes this suite found that the plan does not name.

   The legitimate counterpart of each attack is asserted with plain `is` NEXT
   to the `known-hole`, never inside it: a known hole may only ever document
   an over-permissive answer, not excuse a broken capability."
  (:require [clojure.test :refer [is]]))

(defonce ^{:doc "id → description of every known hole whose attack ran and
                 still succeeded in this JVM."}
  open-holes
  (atom (sorted-map)))

(defn record!
  "Bookkeeping half of `known-hole`: log the outcome and assert it.
   `outcome` is `{:open? bool :threw Throwable?}`."
  [id description outcome]
  (when (:open? outcome)
    (swap! open-holes assoc id description))
  (is (true? (:open? outcome))
      (str "KNOWN HOLE NO LONGER OPEN: " id " — " description
           ". If this was fixed on purpose, flip this `known-hole` into an ordinary"
           " assertion that the attack is refused (and has no effect)."
           (when-let [t (:threw outcome)]
             (str " [the attack threw instead of succeeding: " (ex-message t) "]")))))

(defmacro known-hole
  "Run `attack`, a body that returns truthy iff the attack SUCCEEDED (the hole
   is still open). Passes while it succeeds; fails once it does not — including
   when it throws, since a refusal by exception is also a fix (or a broken
   attack, which deserves a look either way)."
  [id description & attack]
  `(record! ~id ~description
            (try {:open? (boolean (do ~@attack))}
                 (catch Throwable t# {:open? false :threw t#}))))
