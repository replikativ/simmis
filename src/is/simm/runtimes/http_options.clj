(ns is.simm.runtimes.http-options
  "Application HTTP framing options for the maintained HTTP-kit transport.")

(defn server-options [bind-address]
  ;; The handler supplies metadata-only HEAD lengths. HTTP-kit's legacy mode
  ;; replaces them with zero when the body is absent.
  (cond-> {:legacy-content-length? false}
    bind-address (assoc :ip bind-address)))
