(ns is.simm.runtimes.web-url)

(defn websocket-url
  "Derive the WebSocket URL from a page location. Released applications use
   the page origin. Development may explicitly override only the port, keeping
   the browser-visible host and matching ws/wss scheme."
  [{:keys [protocol hostname host]} development-port]
  (let [secure? (= "https:" protocol)
        scheme (if secure? "wss://" "ws://")]
    (if (seq development-port)
      (str scheme (if (seq hostname) hostname "localhost") ":" development-port)
      (str scheme host))))
