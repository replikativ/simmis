(ns is.simm.agents.workflows
  "Bounded pure workflow authoring helpers. No execution or schedule authority."
  (:require [is.simm.documents.links :as links]
            [is.simm.model.candidate-models :as candidate-models]
            [is.simm.documents.workflow-links :as workflow-links]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.wiki-maintenance :as wiki]
            [is.simm.starter.evidence-check :as evidence]
            [is.simm.starter.conversation-digest :as digest]
            [is.simm.starter.daily-room-summary :as daily]))

(defn starter
  "Read a shipped editable program as data. Does not read a Room, install files,
   choose a model, execute, allocate resources or activate a schedule."
  [name]
  (case name
    "evidence-check" {:name name :version evidence/version :files (evidence/source)}
    "maintain-wiki" {:name name :version wiki/version :files (wiki/source)}
    "summarize-room" {:name name :version 1 :files (digest/source)}
    "summarize-room-daily" {:name name :version daily/version :files (daily/source)}
    (throw (ex-info "Choose evidence-check, maintain-wiki, summarize-room or summarize-room-daily" {:type ::unknown-starter}))))

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
           ;; This pure caller has supplied bodies, not a captured store manifest.
           :files (count files)
           :bytes (reduce + 0 (map #(alength (.getBytes ^String % "UTF-8")) (vals files)))
           :setup-status (get-in details [:setup :status])
           :observation :supplied-files :saved-source-verified? false)))

(defn models
  "Installed models a workflow run or benchmark may name, with their reasoning
   efforts. Metadata only: does not call a provider, test a credential, choose
   a model or grant spending."
  []
  (candidate-models/options))

(defn action-links [room name]
  (when-not (and (links/uuid-text? (str room)) (workflow-links/name? name))
    (throw (ex-info "Use a lowercase workflow name with letters, digits and dashes" {})))
  {:room (str room) :name name :scope :canonical-saved-source
   :note "These links open current saved source for human review. Merge proposal edits first. They do not run or schedule anything."
   :run (workflow-links/route {:room (str room) :workflow name :action :run})
   :schedule (workflow-links/route {:room (str room) :workflow name :action :schedule})
   :benchmark (workflow-links/route {:room (str room) :workflow name :action :benchmark})})

(def docs
  '{check-files [([files]) "Inspect a relative workflow file/text map without executing code, reading saved source or granting launch authority. Returns separate ordinary-program and benchmark readiness."
                 [:=> [:cat [:map-of :string :string]] :map]]
    starter [([name]) "Read an editable shipped program as {:name :version :files}. Names: evidence-check (generic task and synthetic benchmark example), maintain-wiki, summarize-room, summarize-room-daily. Does not install or execute."
             [:=> [:cat [:enum "evidence-check" "maintain-wiki" "summarize-room" "summarize-room-daily"]] :map]]
    models [([]) "Installed models a workflow run or benchmark may name: {:models [{:id :label :provider :reasoning-efforts :credential-status} ...] :effort-syntax}. Select an effort as <id>@<effort>, e.g. codex-subscription-luna-6@low; each effort is its own benchmark candidate. Metadata only: no provider call, selection or spending."
            [:=> :cat :map]]
    action-links [([name]) "Room-scoped confirmation links for an existing saved workflow: Run once, Schedule and Benchmark. Merge proposed source first. No execution, scheduling or funding."
                  [:=> [:cat :string] :map]]
    schedules [([]) "List legacy agent schedules in this room as {:id :description}. Saved workflows are files under workflows/<name>/; these schedules are not workflow source."
               [:=> :cat [:vector :map]]]
    set-shape! [([schedule-id mermaid]) "Set a Mermaid overview for an existing legacy schedule owned by this room. This does not define workflow code. For saved workflows, write diagram.mmd beside workflow.edn."
                [:=> [:cat :string :string] :map]]})
