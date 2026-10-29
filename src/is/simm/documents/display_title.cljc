(ns is.simm.documents.display-title
  "Readable document labels; paths and semantic identity remain unchanged."
  (:require [clojure.string :as str]))

(defn title [{:keys [path frontmatter]}]
  (let [label (get-in frontmatter [:properties "title"])]
    (if (and (string? label) (<= 1 (count label) 256)
             (not (str/blank? label)) (not (re-find #"[\u0000-\u001f\u007f]" label)))
      (str/trim label)
      (str/replace (last (str/split (or path "") #"/")) #"(?i)\.(md|markdown)$" ""))))
