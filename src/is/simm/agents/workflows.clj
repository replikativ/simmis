(ns is.simm.agents.workflows
  "Bounded pure workflow authoring helpers. No execution or schedule authority."
  (:require [is.simm.documents.links :as links]
            [is.simm.documents.workflow-links :as workflow-links]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.wiki-maintenance :as wiki]
            [is.simm.starter.conversation-digest :as digest]
            [is.simm.starter.daily-room-summary :as daily]))

(defn starter
  "Read a shipped editable program as data. Does not read a Room, install files,
   choose a model, execute, allocate resources or activate a schedule."
  [name]
  (case name
    "maintain-wiki" {:name name :version wiki/version :files (wiki/source)}
    "summarize-room" {:name name :version 1 :files (digest/source)}
    "summarize-room-daily" {:name name :version daily/version :files (daily/source)}
    (throw (ex-info "Choose maintain-wiki, summarize-room or summarize-room-daily" {:type ::unknown-starter}))))

(defn inspect-files
  "Inspect the supplied relative file/text map only. Does not read a Room,
   evaluate a checker, create an activation or confer launch authority."
  [files]
  (when-not (and (map? files) (<= 1 (count files) workflows/max-files)
                 (every? (fn [[path text]]
                           (and (links/path? path) (string? text)
                                (<= (alength (.getBytes ^String text "UTF-8")) workflows/max-file-bytes))) files)
                 (<= (reduce + 0 (map #(alength (.getBytes ^String % "UTF-8")) (vals files))) workflows/max-bundle-bytes))
    (throw (ex-info "Supply at most 100 relative UTF-8 workflow files, 512 KiB per file and 2 MiB total" {})))
  (let [details (workflows/details-of "draft" {:files files :manifest []})]
    (assoc (workflows/public-inspection (:inspection details))
           :setup-status (get-in details [:setup :status])
           :observation :supplied-files :saved-source-verified? false)))

(defn action-links [room name]
  (when-not (and (links/uuid-text? (str room)) (workflow-links/name? name))
    (throw (ex-info "Use a lowercase workflow name with letters, digits and dashes" {})))
  {:room (str room) :name name :scope :canonical-saved-source
   :note "These links open current saved source for human review. Merge proposal edits first. They do not run or schedule anything."
   :run (workflow-links/route {:room (str room) :workflow name :action :run})
   :schedule (workflow-links/route {:room (str room) :workflow name :action :schedule})
   :benchmark (workflow-links/route {:room (str room) :workflow name :action :benchmark})})
