(ns is.simm.ops.knowledge-citations
  "Pure invocation-specific source addresses, separate from program identity."
  (:require [clojure.string :as str]
            [is.simm.documents.links :as links]
            [is.simm.ops.workflow-policy :as policy]))

(defn- refuse! []
  (throw (ex-info "Invalid retained citation context" {:type ::invalid})))

(defn task [original context]
  (if-not context original
    (do
      (when-not (and (string? original)
                     (= #{:version :room :request} (set (keys context)))
                     (= 1 (:version context)) (uuid? (:room context)) (uuid? (:request context)))
        (refuse!))
      (let [example (links/request-source-route
                     (assoc context :path "/in/knowledge/sources/example.md"))
            endpoint (first (str/split example #"\?" 2))
            rendered (str original "\n\nHOST citation routing: cite original manifest sources using " endpoint
                          "?path= followed by the percent-encoded exact :input path from the frozen manifest. "
                          "These links read the retained source revision under current reader authority; "
                          "they remain valid after proposal adoption. Do not substitute a digest for original evidence.")]
        (when (> (alength (.getBytes ^String rendered "UTF-8")) (:max-task-bytes policy/policy))
          (refuse!))
        rendered))))

(defn semantic-task
  "Verify the invocation suffix and recover the original semantic task for a
   stream's program hash. A new request must not reset evidence progress."
  [input]
  (let [rendered (get-in input [:environment :environment/task])
        context (:knowledge/source-citations input)
        original (:knowledge/source-task input)]
    (if context
      (do (when-not (and (string? original) (= rendered (task original context))) (refuse!))
          original)
      (do (when (contains? input :knowledge/source-task) (refuse!)) rendered))))
