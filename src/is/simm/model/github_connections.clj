(ns is.simm.model.github-connections
  "Closed physical Git connection identity, captured by the fixed HOST.
   A descriptor or digest does not grant access, mutation or cleanup rights."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.documents.github-source :as github]
            [is.simm.model.knowledge-publication :as publication]
            [malli.core :as m]))

(defn- refuse! [reason]
  (throw (ex-info "Git connection identity refused" {:type ::refused :reason reason})))

(defn- reference? [value]
  (and (string? value) (<= 12 (count value) 512) (str/starts-with? value "refs/heads/")
       (not (re-find #"[\\\p{Cntrl}]" value))))

(def connection-schema
  [:map {:closed true} [:version [:= 1]] [:room :uuid]
   [:source [:map {:closed true} [:kind [:= :github]] [:repository [:string {:min 3 :max 256}]]]]
   [:ref [:fn reference?]] [:target-stamp :uuid]
   [:target [:and publication/target-schema [:fn publication/host-target?]]]])

(defn connection! [connection]
  (when-not (and (m/validate connection-schema connection)
                 (= (:source connection) (github/source! (:source connection)))
                 (= (:ref connection) (get-in connection [:target :ref])))
    (refuse! :invalid-connection))
  connection)

(defn connection-id [connection]
  (connection! connection)
  (hasch/uuid [:simmis/github-connection-v1 connection]))
