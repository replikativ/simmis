(ns is.simm.documents.attachments
  "Relative Markdown image dependencies. No network or payload reads."
  (:require [clojure.string :as str]
            [is.simm.documents.links :as links]
            [is.simm.documents.media :as media]))

(def max-images 32)

(defn relative-path [document url]
  (when (and (links/path? document) (string? url) (<= 1 (count url) 2048)
             (not (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*:|^[#/]|[?#\\\u0000-\u0020\u007f]" url)))
    (try
      (let [decoded (links/decode-path url)
            segments (concat (butlast (str/split document #"/")) (str/split decoded #"/" -1))
            path (reduce (fn [out part]
                           (cond (= "." part) out
                                 (= ".." part) (if (seq out) (pop out) (reduced nil))
                                 :else (conj out part))) [] segments)
            result (when path (str/join "/" path))]
        (when (and (links/path? result) (= :image (media/kind result))) result))
      (catch #?(:clj Exception :cljs :default) _ nil))))

(defn images [document analysis]
  (->> (:nodes analysis)
       (mapcat #(tree-seq (comp seq :children) :children %))
       (keep (fn [node]
               (when (#{:Image :ImageRef} (:kind node))
                 (when-let [path (relative-path document (:url node))]
                   {:url (:url node) :path path}))))
       distinct (take max-images) vec))

(defn urls
  "Use only metadata from this view's currently admitted workspace snapshot.
   Unknown/absent dependencies remain inert until a metadata observation arrives."
  [room document analysis entries target]
  (into {} (keep (fn [{:keys [url path]}]
                   (when-let [content (get-in entries [path :content])]
                     (when-let [route (links/media-route {:room room :path path :content content :target target})]
                       [url route]))))
        (images document analysis)))
