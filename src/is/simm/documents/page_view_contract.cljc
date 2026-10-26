(ns is.simm.documents.page-view-contract
  "Portable identities/limits for inert saved page views, never authority.")

(def profile {:id :simmis/page-view :version 1})
(def max-characters 16384)
(def max-columns 16)
(def max-rows 50)
(def max-mounted 4)
