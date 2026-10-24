(ns is.simm.uis.web.desktop.workspace-remote
  "Human canonical workspace creation and read-only intent observation."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.ops.workspace-creation :as creation])))

(defn-spin-remote create-workspace!
  [server-id request]
  (spin-remote server-id [request]
               #?(:clj (let [body (creation/prepare (access/authenticated-party-id) :create request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))

(defn-spin-remote observe-workspace-creation!
  [server-id request]
  (spin-remote server-id [request]
               #?(:clj (let [body (creation/prepare (access/authenticated-party-id) :observe request)]
                         (await (blocking/blocking body)))
                  :cljs nil)))
