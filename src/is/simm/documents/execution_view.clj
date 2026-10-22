(ns is.simm.documents.execution-view
  "Host-installed source roles in disposable knowledge execution worlds.
   This is selection metadata, never permission or an adoption decision."
  (:require [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [org.replikativ.spindel.engine.component :as component]
            [org.replikativ.spindel.engine.core :as ec]
            [yggdrasil.protocols :as systems]))

(def profile {:id :simmis/knowledge-execution :version 1})
(def ^:private component-id ::source-role)
(def ^:private exclusions ["in/knowledge" "out/wiki"])

(defn- source-identity [system conn]
  {:system (str (systems/system-id system))
   :repository (:geschichte.repo/id (query/repository @conn))})

(defn install!
  "Install only after successful staging, while bound to the disposable world.
   Immutable component metadata follows forks/copies but is not merged into
   the parent by Yggdrasil system settlement. No native handles are retained."
  [world]
  (let [ctx (ec/current-execution-context)]
    (when-not (and (identical? ctx (:ctx world)) (:parent-ctx ctx))
      (throw (ex-info "Execution source roles require a bound disposable world" {})))
    (let [{:keys [system conn]} (workspace/current-workspace)
          source (source-identity system conn)
          old (get (component/registered) component-id)]
      (when-not (:repository source)
        (throw (ex-info "Execution source role requires a repository identity" {})))
      (if old
        (when-not (and (= profile (:profile old)) (= source (:source old)))
          (throw (ex-info "Execution source role belongs to another repository" {})))
        (component/register! component-id
                             (component/shared {:profile profile :source source :scope (random-uuid)})))
      profile)))

(defn selection-options
  "Apply the selected world's closed host role AFTER normal source authority
   and world checks. Authored files and callers cannot supply exclusions.
   Raw file access is unchanged. A mismatched role refuses rather than widening."
  [system conn options]
  (if-let [role (get (component/registered) component-id)]
    (do
      (when-not (and (= #{:profile :source :scope} (set (keys role)))
                     (= profile (:profile role)) (uuid? (:scope role))
                     (= (source-identity system conn) (:source role)))
        (throw (ex-info "Execution source role does not match the selected repository" {})))
      (-> options
          (update :exclude-prefixes #(vec (distinct (concat (or % []) exclusions))))
          (update :schema-revision #(vector % profile))))
    options))
