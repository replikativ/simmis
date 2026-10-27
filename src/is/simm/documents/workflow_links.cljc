(ns is.simm.documents.workflow-links
  "Room-scoped workflow navigation. Links reveal setup; they never execute it."
  (:require [clojure.string :as str]
            [is.simm.documents.links :as memory]))

(def actions #{:run :schedule :benchmark})
(defn name? [value]
  (and (string? value) (boolean (re-matches #"[a-z0-9][a-z0-9-]{0,63}" value))))

(defn route [{:keys [room workflow action]}]
  (when (and (memory/uuid-text? room) (name? workflow) (contains? actions action))
    (str "/room/" room "/workflows/" workflow "/" (name action))))

(defn parse-route [segments]
  (let [[_ room _ workflow action] segments
        ref {:kind :workflow :room room :workflow workflow
             :action (get {"run" :run "schedule" :schedule "benchmark" :benchmark} action)}]
    (when (and (= 5 (count segments)) (= "room" (first segments))
               (= "workflows" (nth segments 2)) (route ref)) ref)))

(defn file-route
  "Resolve repository-relative chat links against the message's own room.
   Explicit application routes, external URLs, fragments and ambiguous paths
   remain untouched. A link does not grant access to its destination."
  [room href]
  (when (and (memory/uuid-text? room) (string? href) (<= 1 (count href) 2048)
             (not (re-find #"^(?:[A-Za-z][A-Za-z0-9+.-]*:|//|#)" href))
             (not (re-find #"[?#]" href))
             (not (re-find #"^/(?:room|page|proposal|auth|api)(?:/|$)" href)))
    (let [path (some-> href memory/decode-path (str/replace #"^/" ""))]
      (when (memory/path? path)
        (memory/route {:kind :memory-file :room room :path path})))))

(defn mentioned-workflows
  "Bounded link-only discovery for chat action cards; prose/code is not an
   instruction to execute. Returned names still require saved-source checks."
  [content]
  (->> (re-seq #"\]\((?:/)?workflows/([a-z0-9][a-z0-9-]{0,63})/[^\s)]*\)" (subs (str content) 0 (min 65536 (count (str content)))))
       (map second) distinct (take 4) vec))
