(ns is.simm.documents.github-source
  "Closed public GitHub identities. No credentials, arbitrary hosts or refs."
  (:require [clojure.string :as str]))

(defn repository!
  "Normalize an owner/repository or HTTPS github.com repository URL. The result
   is an inert identity; only the fixed HOST importer may turn it into IO."
  [value]
  (let [text (when (and (string? value) (<= 1 (count value) 256)) value)
        path (when text
               (or (second (re-matches #"https://github\.com/([^?#]+?)/?" text))
                   (when-not (str/includes? text ":") text)))
        path (some-> path str/lower-case (str/replace #"\.git$" ""))
        parts (when path (str/split path #"/" -1))
        [owner repository] parts]
    (when-not (and (= 2 (count parts))
                   (re-matches #"[A-Za-z0-9](?:[A-Za-z0-9-]{0,37}[A-Za-z0-9])?" (or owner ""))
                   (not (str/includes? owner "--"))
                   (<= 1 (count repository) 100)
                   (re-matches #"[A-Za-z0-9_.-]+" repository)
                   (not (str/ends-with? repository ".git"))
                   (not (contains? #{"." ".."} repository)))
      (throw (ex-info "Use a public GitHub owner/repository or HTTPS repository URL."
                      {:type ::invalid-source})))
    (str owner "/" repository)))

(defn source!
  "Validate the complete source descriptor retained in a creation intent."
  [source]
  (when-not (and (map? source) (= #{:kind :repository} (set (keys source)))
                 (= :github (:kind source)))
    (throw (ex-info "Invalid GitHub source descriptor" {:type ::invalid-source})))
  {:kind :github :repository (repository! (:repository source))})

(defn public-url [source]
  (str "https://github.com/" (:repository (source! source)) ".git"))
