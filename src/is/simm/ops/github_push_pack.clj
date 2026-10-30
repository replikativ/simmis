(ns is.simm.ops.github-push-pack
  "Fixed HOST binary pack persistence. A typed control store-ref must be
   published synchronously while the unreferenced-write guard is held. Pack
   bytes never pass through the EDN artifact codec or an untrusted wire API."
  (:require [datahike.blob :as blob]
            [datahike.gc-guard :as guard]
            [datahike.store :as store]
            [geschichte.budget :as budget]
            [is.simm.model.github-push :as push]
            [konserve.core :as k])
  (:import [java.io InputStream]))

(defn- refuse! []
  (throw (ex-info "Stored Git pack is unavailable or invalid" {:type ::refused :reason :pack-unavailable})))

(defn- size? [n] (and (integer? n) (<= 1 n push/max-pack-bytes)))

(defn publish!
  "Write bounded native bytes and synchronously publish their typed store-ref.
   The callback is fixed HOST code, never request-selected. A failure can leave
   an unreferenced blob but cannot establish publication or dispatch rights."
  [conn value publish]
  (when-not (and (bytes? value) (size? (alength ^bytes value)) (fn? publish)) (refuse!))
  (let [db @conn ref (blob/blob-id value)]
    (guard/with-unreferenced-writes (store/canonical-store-id (:store db) (get-in db [:config :store]))
      ;; Refresh deduplicated blobs under the guard as well.
      (k/bassoc (:store db) ref value {:sync? true})
      (publish ref))))

(defn read!
  "Read exactly the admitted size, then verify its content-addressed identity.
   Konserve owns a borrowed stream's cleanup. Its backend I/O is not preemptible;
   the HOST retains physical operation ownership until this callback exits."
  [conn ref expected-size operation]
  (budget/require-limits! operation [:pack-bytes])
  (when-not (and (uuid? ref) (size? expected-size)) (refuse!))
  (budget/charge! operation :pack-bytes expected-size)
  (let [db @conn]
    (k/bget (:store db) ref
            (fn [{:keys [input-stream]}]
              (budget/check! operation)
              (let [value (cond
                            (bytes? input-stream) input-stream
                            (instance? InputStream input-stream)
                            (let [value (.readNBytes ^InputStream input-stream (int expected-size))]
                              (when-not (neg? (.read ^InputStream input-stream)) (refuse!))
                              value)
                            :else (refuse!))]
                (when-not (= expected-size (alength ^bytes value)) (refuse!))
                (budget/check! operation)
                (when-not (= ref (blob/blob-id value)) (refuse!))
                (budget/check! operation)
                value))
            {:sync? true})))
