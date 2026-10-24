(ns is.simm.documents.links
  "Portable, data-only links to owned room files and explicit semantic blocks.
   Link identities constrain a read; they never convey permissions."
  (:require [clojure.string :as str])
  #?(:clj (:import [java.net URLEncoder URLDecoder]
                   [java.nio ByteBuffer]
                   [java.nio.charset StandardCharsets CodingErrorAction])))

(defn- code-at [text index]
  #?(:clj (int (.charAt ^String text index)) :cljs (.charCodeAt ^js text index)))

(defn- unicode? [text]
  (loop [index 0]
    (if (= index (count text)) true
        (let [code (code-at text index)]
          (cond
            (<= 0xd800 code 0xdbff)
            (and (< (inc index) (count text))
                 (<= 0xdc00 (code-at text (inc index)) 0xdfff)
                 (recur (+ index 2)))
            (<= 0xdc00 code 0xdfff) false
            :else (recur (inc index)))))))

(defn path? [path]
  (and (string? path) (<= 1 (count path) 512) (unicode? path)
       (not (re-find #"[\\\u0000-\u001f\u007f]" path))
       (every? #(and (not (str/blank? %)) (not (#{"." ".."} %))) (str/split path #"/" -1))))

(defn uuid-text? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" value))))

(defn- encode [text]
  #?(:clj (-> (URLEncoder/encode ^String text "UTF-8") (str/replace "+" "%20") (str/replace "%7E" "~"))
     :cljs (str/replace (js/encodeURIComponent text) #"[!'()]"
                       #(str "%" (.toUpperCase (.toString (code-at % 0) 16))))))

(defn- decode [raw]
  (try
    (let [raw (str/replace raw "+" "%20")
          ;; URLDecoder replaces malformed UTF-8, unlike decodeURIComponent.
          ;; Validate each percent-byte run before using the standard decoder.
          _ #?(:clj
               (doseq [run (re-seq #"(?:%[0-9a-fA-F]{2})+" raw)]
                 (let [bytes (byte-array (map #(unchecked-byte (Integer/parseInt (subs % 1) 16))
                                             (re-seq #"%[0-9a-fA-F]{2}" run)))
                       decoder (doto (.newDecoder StandardCharsets/UTF_8)
                                 (.onMalformedInput CodingErrorAction/REPORT)
                                 (.onUnmappableCharacter CodingErrorAction/REPORT))]
                   (.decode decoder (ByteBuffer/wrap bytes))))
               :cljs nil)
          decoded #?(:clj (URLDecoder/decode ^String raw "UTF-8") :cljs (js/decodeURIComponent raw))]
      (when (unicode? decoded) decoded))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(defn- parameters [query]
  (when (and (string? query) (<= 1 (count query) 8192))
    (reduce
     (fn [out field]
       (let [[raw-key raw-value] (str/split field #"=" 2)
             key (decode raw-key) value (when (some? raw-value) (decode raw-value))]
         (if (and (#{"path" "repo" "block"} key) (some? value) (not (contains? out key)))
           (assoc out key value) (reduced nil))))
     {} (str/split query #"&" -1))))

(defn parse-query [room query]
  (when (uuid-text? room)
    (let [fields (parameters query) field-keys (set (keys fields)) path (get fields "path")
          repository (get fields "repo") block (get fields "block")]
      (cond
        (and (= #{"path"} field-keys) (path? path))
        {:kind :memory-file :room room :path path}
        (and (#{#{"repo" "block"} #{"repo" "block" "path"}} field-keys)
             (uuid-text? repository) (uuid-text? block) (or (not (contains? fields "path")) (path? path)))
        (cond-> {:kind :memory-block :room room :repository repository :block block} path (assoc :path path))
        :else nil))))

(defn route [{:keys [kind room path repository block]}]
  (let [room (str room) repository (some-> repository str) block (some-> block str)]
    (when (uuid-text? room)
      (case kind
        :memory-file (when (path? path) (str "/room/" room "/files?path=" (encode path)))
        :memory-block (when (and (uuid-text? repository) (uuid-text? block)
                                 (or (nil? path) (path? path)))
                        (str "/room/" room "/files?repo=" repository "&block=" block
                             (when path (str "&path=" (encode path)))))
        nil))))

(defn parse-route
  "Only exact origin-relative canonical file/block addresses. An external URL
   or an unrecognized query is never reinterpreted as a local reference."
  [route]
  (when (and (string? route) (<= (count route) 9000))
    (when-let [[_ room query] (re-matches #"/room/([^/?#]+)/files\?([^#]+)" route)]
      (parse-query room query))))

(def ^:private source-prefix "/in/knowledge/sources/")

(defn source-path? [path]
  (and (string? path) (str/starts-with? path source-prefix)
       (path? (subs path (count source-prefix)))))

(defn source-route [{:keys [proposal component path]}]
  (let [proposal (str proposal) component (str component)]
    (when (and (uuid-text? proposal) (uuid-text? component) (source-path? path))
      (str "/proposal/" proposal "/source/" component "?path=" (encode path)))))

(defn parse-source-query [proposal component query]
  (let [fields (parameters query) path (get fields "path")]
    (when (and (uuid-text? proposal) (uuid-text? component)
               (= #{"path"} (set (keys fields))) (source-path? path))
      {:kind :proposal-source :proposal proposal :component component :path path})))
