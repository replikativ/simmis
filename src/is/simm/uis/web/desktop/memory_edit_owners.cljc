(ns is.simm.uis.web.desktop.memory-edit-owners
  "Pure cross-tab draft/write observations over existing context-owned Memory
   state. These UI guards grant no authority and replace no source CAS check."
  (:require [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-declarations :as declarations]))

(defn- same-target? [key state other-key other]
  (and (vector? key) (vector? other-key) (not= key other-key)
       (= (take 2 key) (take 2 other-key))
       (uuid? (:target-stamp state)) (= (:target-stamp state) (:target-stamp other))))

(defn conflicts
  "Other buffers belonging to this session/Room/Memory which must be retained.
   Caller supplies the whole signal value from its own execution context; no
   global registry, lock, connection or native editor handle is retained here."
  [states key path]
  (let [state (get states key)]
    (into []
          (keep (fn [[other-key other]]
                  (when (same-target? key state other-key other)
                    (when-let [reason (cond
                                        (declarations/busy? other path) :pending-write
                                        (editor/dirty? (get-in other [:drafts path])) :source-draft
                                        (editor/property-drafts? other path) :property-draft
                                        (declarations/drafts? other path) :type-draft)]
                      {:owner other-key :reason reason}))))
          states)))

(defn blocked? [states key path]
  (boolean (seq (conflicts states key path))))

(def message "Another tab has unsaved edits or a save in progress for this file. Finish or discard those edits before changing its properties here.")
