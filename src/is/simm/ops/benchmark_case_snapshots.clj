(ns is.simm.ops.benchmark-case-snapshots
  "HOST-only retained-case reads through the original private fixture store.
   Callers retain native lifetime and authorization. No branch is connected,
   store configuration instantiated, pin created or execution admitted here."
  (:require [datahike.api :as d]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.knowledge-changes :as changes]))

(defn- refuse! [reason]
  (throw (ex-info "Retained benchmark case is unavailable" {:type ::held :reason reason})))

(defn load-case-snapshot!
  "Read a builder's immutable commit after its temporary branch was retired.
   Both bases are retained HOST data, not authority. The original fixture DB
   and its full C0 basis must still match. Resolve only through that DB's raw
   store, then check the complete stored case basis, including its original
   branch/configuration. Existing physical pin verification remains separate."
  [fixture-db fixture-basis case-basis]
  (streams/validate-basis! fixture-basis)
  (streams/validate-basis! case-basis)
  (when-not (= fixture-basis (changes/basis fixture-db))
    (refuse! :fixture-changed))
  (when-not (= (select-keys fixture-basis [:store :backend :repository])
               (select-keys case-basis [:store :backend :repository]))
    (refuse! :case-store-changed))
  (let [snapshot (d/commit-as-db (:store fixture-db) (:commit case-basis))]
    (when-not snapshot (refuse! :case-unavailable))
    (when-not (= case-basis (changes/basis snapshot))
      (refuse! :case-basis-changed))
    snapshot))
