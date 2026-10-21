(ns is.simm.documents.identity
  "One inert document-identity codec shared by query views and file editors."
  (:require [hasch.core :as hasch]))

(defn canonical-uuid [raw]
  (when (string? raw)
    (try
      (let [id (java.util.UUID/fromString raw)] (when (= raw (str id)) id))
      (catch IllegalArgumentException _ nil))))

(defn document [repository-id path parsed]
  (if-let [entry (find (get-in parsed [:frontmatter :properties]) "simmis-id")]
    (let [raw (val entry)
          id (canonical-uuid raw)]
      (if id
        {:id id :identity-status :explicit :diagnostics []}
        {:identity-status :invalid :diagnostics [{:kind :invalid-document-id :path path}]}))
    {:id (hasch/uuid [:source-document repository-id path])
     :identity-status :provisional :diagnostics []}))
