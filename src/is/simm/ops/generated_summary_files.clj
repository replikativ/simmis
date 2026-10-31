(ns is.simm.ops.generated-summary-files
  "Generated ownership comes from native delivery metadata, never authored
   frontmatter. Applies to normal file editors; Git divergence stays inspectable."
  (:require [is.simm.model.summary-records :as records]
            [org.replikativ.spindel.engine.core :as ec]))

(defn inspect [room room-id target path entry]
  (when-let [conn (:conn (:store room))]
    (binding [ec/*execution-context* (:ctx room)]
      (when (records/installed? @conn)
        (when-let [owner (records/path-in @conn (records/path-id room-id (records/target-id target) path))]
          (let [projection (records/projection-in @conn (:summary.path/owner owner))]
            (when (#{:markdown :transcript} (:summary.projection/kind projection))
              {:read-only? true :kind (:summary.projection/kind projection)
               :revision (:summary.projection/revision projection)
               :status (cond
                         (not= :published (:summary.path/state owner)) :inspection-required
                         (not= (:summary.path/content owner) (:content entry)) :diverged
                         :else :current)})))))))

(defn editable! [room room-id target path entry]
  (when (inspect room room-id target path entry)
    (throw (ex-info "Generated summary views are read only; copy them into an authored note to edit."
                    {:type ::read-only :path path}))))
