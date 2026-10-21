(ns is.simm.authority.rpc-wire
  "Exercise the public inbound middleware and server, not private dispatch vars."
  (:require [clojure.core.async :as a]
            [kabel.peer :as peer]
            [kabel.remote :as remote]
            [superv.async :as sv]))

(defn invoke!
  [principal fn-name arg-map authorize]
  (let [supervisor (sv/dummy-supervisor)
        errors-done (a/go-loop []
                      (when-let [error (a/<! (sv/-error supervisor))]
                        (sv/-free-exception supervisor error)
                        (recur)))
        server (peer/client-peer supervisor (random-uuid) remote/middleware identity)
        served (remote/serve server {:authorize-fn authorize})
        in (a/chan 4) out (a/chan 4)
        remote-id (random-uuid)
        request-id (random-uuid)
        take! (fn []
                (let [[value port] (a/alts!! [out (a/timeout 30000)])]
                  (when-not (= out port)
                    (throw (ex-info "Inbound invocation produced no reply" {:fn fn-name})))
                  value))]
    (try
      (peer/drain (remote/middleware [supervisor server [in out]]))
      (take!) ;; initial modern announcement
      (a/>!! in {:type :is.simm.distributed-scope/register-scope :scope remote-id})
      (take!) ;; negotiated legacy announcement
      ;; This fixture represents the boundary after authentication has stamped
      ;; the principal; authorization and handler binding remain the real code.
      (a/>!! in {:type :is.simm.distributed-scope/invoke
                 :scope (:id @server) :request-scope remote-id :request-id request-id
                 :fn-name fn-name :arg-map arg-map :kabel/principal principal})
      (let [reply (take!)]
        (when-not (= request-id (:request-id reply))
          (throw (ex-info "Unexpected inbound reply" {:reply reply})))
        reply)
      (finally
        ((:stop! served))
        (a/close! in)
        (let [joined (a/promise-chan)
              registered (:registered supervisor)
              watch-key (random-uuid)
              complete! #(when (empty? @registered) (a/put! joined true))]
          (add-watch registered watch-key (fn [& _] (complete!)))
          (try
            (doseq [ch (:aborts supervisor)] (a/put! ch :abort) (a/close! ch))
            (complete!)
            (let [[_ port] (a/alts!! [joined (a/timeout 10000)])]
              (when-not (= joined port)
                (throw (ex-info "RPC fixture failed to join" {:registered @registered}))))
            (a/close! (sv/-error supervisor))
            (a/<!! errors-done)
            (finally
              (remove-watch registered watch-key)
              (a/close! (first (get-in @server [:volatile :chans])))
              (peer/unregister-peer! (:id @server)))))))))
