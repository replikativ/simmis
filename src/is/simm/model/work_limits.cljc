(ns is.simm.model.work-limits
  "Closed bounded-work invocation limits. Pure data, without admission or
   execution dependencies, so retained receipts can validate their old limits.")

(def schema
  [:map {:closed true}
   [:budget-microdollars [:int {:min 1 :max 5000000}]]
   [:max-dispatches [:int {:min 1 :max 32}]]
   [:max-model-steps [:int {:min 1 :max 32}]]
   [:timeout-ms [:int {:min 1000 :max 600000}]]
   [:settlement [:enum "review" "discard"]]])
