(ns is.simm.uis.web.desktop.captured-result
  "Pure validation of a recorded file's bounded, contiguous byte stream."
  (:require [clojure.string :as str]))

(def max-download-bytes (* 8 1024 1024))

(defn- byte-chunk-valid? [maximum entry offset chunk decoded-length]
  (let [size (:size entry) n (:length chunk) end (when (and (integer? n) (integer? offset)) (+ offset n))]
    (and (= (:path entry) (:path chunk)) (= (:content-id entry) (:content-id chunk))
         (= size (:size chunk)) (integer? size) (<= 0 size maximum)
         (= "base64" (:encoding chunk)) (= offset (:offset chunk))
         (integer? offset) (integer? n) (<= 0 n 16384) (= n decoded-length)
         (<= 0 offset end size)
         (if (< end size)
           (and (pos? n) (= end (:next-offset chunk)))
           (nil? (:next-offset chunk))))))

(defn chunk-valid? [attempt-id entry offset chunk decoded-length]
  (and (string? attempt-id) (= attempt-id (get-in chunk [:attempt :id]))
       (byte-chunk-valid? max-download-bytes entry offset chunk decoded-length)))

(defn- uuid-text? [value]
  (and (string? value)
       (boolean (re-matches #"(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" value))))

(def source-identity-keys [:proposal :component :request :selection])
(def max-source-bytes 524288)

(defn- source-files-valid? [listing & [summary?]]
  (let [files (:files listing) selection (:selection listing)
        selection-keys (if summary? #{:coverage-id :context-id :manifest-id} #{:trigger-id :execution-id :manifest-id})
        prefix (if summary? "/in/summary/sources/" "/in/knowledge/sources/")]
    (and (map? listing) (map? selection)
         (= selection-keys (set (keys selection)))
         (every? uuid-text? (vals selection))
         (vector? files) (<= (count files) 100)
         (= (count files) (count (set (map :path files))))
         (every? (fn [{:keys [path source-path role size content-id]}]
                   (and (string? source-path) (<= 1 (count source-path) 512)
                        (= path (str prefix source-path))
                        ((if summary? #{:coverage :context} #{:trigger :context}) role)
                        (integer? size) (<= 0 size max-source-bytes)
                        (uuid-text? content-id))) files))))

(defn source-list-valid? [proposal component listing]
  (and (= proposal (:proposal listing)) (= component (:component listing))
       (every? uuid-text? [proposal component (:request listing)])
       (source-files-valid? listing)))

(defn request-source-list-valid? [room request listing]
  (let [summary? (= :room-summary (:kind listing))]
    (and (= (if summary? #{:kind :revision :room :request :selection :files} #{:room :request :selection :files})
            (set (keys listing)))
       (or (not summary?) (uuid-text? (:revision listing)))
       (= room (:room listing)) (= request (:request listing))
       (every? uuid-text? [room request]) (source-files-valid? listing summary?))))

(defn request-source-chunk-valid? [listing entry offset chunk decoded-length]
  (and (map? listing) (map? chunk)
       (= (select-keys listing [:kind :revision]) (select-keys chunk [:kind :revision]))
       (= (select-keys listing [:room :request :selection])
          (select-keys chunk [:room :request :selection]))
       (byte-chunk-valid? max-source-bytes entry offset chunk decoded-length)))

(defn source-chunk-valid? [listing entry offset chunk decoded-length]
  (and (map? listing) (map? chunk)
       (= (select-keys listing source-identity-keys)
          (select-keys chunk source-identity-keys))
       (byte-chunk-valid? max-source-bytes entry offset chunk decoded-length)))

(defn download-name [path]
  (let [leaf (last (str/split (str path) #"[/\\]"))
        safe (str/replace (or leaf "") #"[\x00-\x1f\x7f:]" "_")]
    (if (or (str/blank? safe) (contains? #{"." ".."} safe)) "recorded-output" safe)))
