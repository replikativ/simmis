(ns is.simm.documents.media
  "Inert media classification and immutable file references, shared by host/UI."
  (:require [clojure.string :as str]
            [is.simm.documents.links :as links]))

(def formats
  {"png" [:image "image/png"] "jpg" [:image "image/jpeg"] "jpeg" [:image "image/jpeg"]
   "gif" [:image "image/gif"] "webp" [:image "image/webp"]
   "mp4" [:video "video/mp4"] "webm" [:video "video/webm"] "ogv" [:video "video/ogg"]
   "mp3" [:audio "audio/mpeg"] "m4a" [:audio "audio/mp4"] "ogg" [:audio "audio/ogg"]
   "wav" [:audio "audio/wav"] "flac" [:audio "audio/flac"]
   "pdf" [:pdf "application/pdf"]})

(defn format-of [path]
  (when (string? path)
    (get formats (some-> (re-find #"\.([^./]+)$" path) second str/lower-case))))

(defn kind [path] (first (format-of path)))
(defn content-type [path] (second (format-of path)))

(defn reference
  "Portable evidence identity. A path is a readable locator; the content ID
   identifies the inspected bytes. Neither this value nor its hash grants access."
  [repository path entry]
  (when-not (and (uuid? repository) (links/path? path) (uuid? (:content entry))
                 (integer? (:size entry)) (<= 0 (:size entry)) (kind path))
    (throw (ex-info "Invalid media reference" {:type ::invalid-reference})))
  {:repository repository :path path :content (:content entry) :size (:size entry)})
