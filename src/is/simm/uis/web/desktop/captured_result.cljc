(ns is.simm.uis.web.desktop.captured-result
  "Pure validation of a recorded file's bounded, contiguous byte stream."
  (:require [clojure.string :as str]))

(def max-download-bytes (* 8 1024 1024))

(defn chunk-valid? [attempt-id entry offset chunk decoded-length]
  (let [size (:size entry) n (:length chunk) end (when (and (integer? n) (integer? offset)) (+ offset n))]
    (and (string? attempt-id) (= attempt-id (get-in chunk [:attempt :id]))
         (= (:path entry) (:path chunk)) (= (:content-id entry) (:content-id chunk))
         (= size (:size chunk)) (integer? size) (<= 0 size max-download-bytes)
         (= "base64" (:encoding chunk)) (= offset (:offset chunk))
         (integer? offset) (integer? n) (<= 0 n 16384) (= n decoded-length)
         (<= 0 offset end size)
         (if (< end size)
           (and (pos? n) (= end (:next-offset chunk)))
           (nil? (:next-offset chunk))))))

(defn download-name [path]
  (let [leaf (last (str/split (str path) #"[/\\]"))
        safe (str/replace (or leaf "") #"[\x00-\x1f\x7f:]" "_")]
    (if (or (str/blank? safe) (contains? #{"." ".."} safe)) "recorded-output" safe)))
