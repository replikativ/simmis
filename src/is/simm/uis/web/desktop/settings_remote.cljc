(ns is.simm.uis.web.desktop.settings-remote
  "Spin-remote functions for party settings."
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            #?(:clj [is.simm.model.parties :as parties])
            #?(:clj [is.simm.model.knowledge-bases :as kbs])
            #?(:clj [is.simm.model.mail-accounts :as mail])
            #?(:clj [is.simm.model.model-catalog :as model-catalog])))

#?(:clj
   (defn load-settings-server [party-id-str]
     (let [party-id (java.util.UUID/fromString party-id-str)]
       {:profile (-> (parties/get-party party-id)
                     (update :party/created #(when % (str %)))
                     (update :party/last-login #(when % (str %))))
        :env-vars (parties/get-env-vars party-id)
        :budget (parties/get-budget party-id)
        :ui-prefs (parties/get-ui-prefs party-id)
        :mail-accounts (mail/list-accounts party-id)
        ;; Built server-side from the same authoritative availability result
        ;; resolution uses. Curated rows stay visible; unavailable ones carry
        ;; their disabled state and explanation.
        :model-choices (model-catalog/choices)
        :knowledge-bases (->> (kbs/get-party-kbs party-id)
                              (mapv (fn [kb]
                                      (-> kb
                                          (update :kb/created #(when % (str %)))))))})))

#?(:clj
   (defn save-model-server [party-id-str model-id]
     (let [party-id (java.util.UUID/fromString party-id-str)]
       ;; Browser disabled state is presentation, not authority. Recompute on
       ;; the server immediately before writing so a forged/stale RPC fails
       ;; closed too.
       (model-catalog/require-available-choice! model-id)
       (parties/update-preferred-model! party-id model-id)
       {:success true})))

#?(:clj
   (defn save-env-var-server [party-id-str env-key value]
     (let [party-id (java.util.UUID/fromString party-id-str)]
       (parties/set-env-var! party-id env-key value)
       {:success true})))

#?(:clj
   (defn delete-env-var-server [party-id-str env-key]
     (let [party-id (java.util.UUID/fromString party-id-str)]
       (parties/delete-env-var! party-id env-key)
       {:success true})))

(defn-spin-remote load-settings!
  [server-id party-id-str]
  (spin-remote server-id [party-id-str]
    (let [pid (identity party-id-str)]
      #?(:clj (load-settings-server pid)
         :cljs nil))))

(defn-spin-remote save-preferred-model!
  [server-id party-id-str model-id]
  (spin-remote server-id [party-id-str model-id]
    (let [pid (identity party-id-str)
          mid (identity model-id)]
      #?(:clj (save-model-server pid mid)
         :cljs nil))))

(defn-spin-remote save-env-var!
  [server-id party-id-str env-key value]
  (spin-remote server-id [party-id-str env-key value]
    (let [pid (identity party-id-str)
          k (identity env-key)
          v (identity value)]
      #?(:clj (save-env-var-server pid k v)
         :cljs nil))))

(defn-spin-remote save-syntax-pref!
  [server-id party-id-str syntax-kw]
  (spin-remote server-id [party-id-str syntax-kw]
    (let [pid (identity party-id-str)
          kw  (identity syntax-kw)]
      #?(:clj (do (parties/set-ui-pref! (java.util.UUID/fromString pid) :ui-pref/syntax kw)
                  {:success true})
         :cljs nil))))

(defn-spin-remote delete-env-var!
  [server-id party-id-str env-key]
  (spin-remote server-id [party-id-str env-key]
    (let [pid (identity party-id-str)
          k (identity env-key)]
      #?(:clj (delete-env-var-server pid k)
         :cljs nil))))
