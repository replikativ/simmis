(ns is.simm.agents.workflows
  "Bounded pure workflow authoring helpers. No execution or schedule authority."
  (:require [is.simm.documents.links :as links]
            [is.simm.documents.workflow-links :as workflow-links]
            [is.simm.ops.workflows :as workflows]))

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
  (assoc (workflows/public-inspection
          (:inspection (workflows/details-of "draft" {:files files :manifest []})))
         :observation :supplied-files :saved-source-verified? false))

(defn action-links [room name]
  (when-not (and (links/uuid-text? (str room)) (workflow-links/name? name))
    (throw (ex-info "Use a lowercase workflow name with letters, digits and dashes" {})))
  {:room (str room) :name name :scope :canonical-saved-source
   :note "These links open current saved source for human review. Merge proposal edits first. They do not run or schedule anything."
   :run (workflow-links/route {:room (str room) :workflow name :action :run})
   :schedule (workflow-links/route {:room (str room) :workflow name :action :schedule})
   :benchmark (workflow-links/route {:room (str room) :workflow name :action :benchmark})})
