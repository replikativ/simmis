(ns is.simm.documents.saved-view-contract
  "Portable identities/limits for inert saved views, never authority.")

(def profile {:id :simmis/saved-view :version 1})
(def max-characters 16384)
(def max-columns 16)
(def max-rows 50)
(def max-mounted 4)

(def max-scorecard-rows 32)
(def max-candidate-characters 256)

(def scorecard-columns
  {:candidate "Candidate" :attempts "Attempts" :passed "Passed"
   :pass-rate "Pass rate" :pass-rate-interval "95% pass range"
   :reward "Mean reward" :reward-interval "95% reward range"
   :cost "Model cost" :notional-cost "At list price"
   :cost-per-pass "Cost per pass" :notional-cost-per-pass "List-price per pass"
   :cost-known "Cost known" :notional-cost-known "List price known"
   :usage-reported "Usage reported"})
