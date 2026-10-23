(ns is.simm.documents.chart-contract
  "Portable fixed feature identities and limits. These are not authority:
   HOST activation still validates actual complete installed definitions.")

(def profile {:id :simmis/typed-chart :version 1})
(def data-profile {:id :simmis/chart-data :version 1})
(def type-id #uuid "2d4a6ef1-5159-45d6-b12d-683064cb8cee")
(def property-id #uuid "0bcb85c0-405d-46b2-90b9-d34d81351975")
(def max-mounted 4)
(def max-bytes 65536)
(def max-points 2000)
(def max-series 8)
