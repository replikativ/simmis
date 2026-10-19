(ns is.simm.documents.evaluation
  "Human SCI sessions, owned by the room's execution world, not an agent.
   Saving source never calls this namespace. No room tools are injected."
  (:require [dvergr.sandbox :as sandbox]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.runtimes.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.signal :refer [signal]]))

(defonce ^:private sessions (signal context/server-context {}))

(defn- form-range [text cursor]
  (let [r (readers/indexing-push-back-reader text)
        matcher (re-matcher #"\r\n|\r|\n" text)
        starts (loop [v [0]] (if (.find matcher) (recur (conj v (.end matcher))) v))
        position #(+ (nth starts (dec (readers/get-line-number r)))
                     (dec (readers/get-column-number r)))
        eof (Object.)]
    (binding [reader/*read-eval* false reader/*data-readers* {}]
      (loop [from 0]
        (let [form (reader/read {:eof eof :read-cond :allow :features #{:clj}} r) to (position)]
          (when (identical? form eof)
            (throw (ex-info "No top-level form at the cursor" {:type ::invalid})))
          (if (<= from cursor to) [from to] (recur to)))))))

(defn request-code [{:keys [path text from to scope] :as request}]
  (files/path! path)
  (when-not (and (= #{:path :text :from :to :scope} (set (keys request)))
                 (re-find #"(?i)\.(clj|cljc|bb)$" path)
                 (string? text) (<= (count text) 65536)
                 (#{:selection :file :form} scope)
                 (integer? from) (integer? to) (<= 0 from to (count text)))
    (throw (ex-info "Select Clojure source to evaluate (maximum 64K characters)" {:type ::invalid})))
  ;; Also reject broken UTF-16 boundaries rather than evaluating altered text.
  (let [[from to] (case scope :file [0 (count text)] :form (form-range text from) [from to])
        code (subs text from to)]
    (when (str/blank? code) (throw (ex-info "Select some Clojure source" {:type ::invalid})))
    (markdown/source-bytes code)
    {:from from :to to :code code}))

(defn- clipped [value]
  (let [s (str value)] (if (> (count s) 12000) (str (subs s 0 12000) "\n… [truncated]") s)))

(defn- file-namespace [text]
  (binding [reader/*read-eval* false reader/*data-readers* {}]
    (let [form (reader/read {:eof nil :read-cond :allow :features #{:clj}}
                            (readers/indexing-push-back-reader text))]
      (when (and (seq? form) (= 'ns (first form)) (symbol? (second form)))
        (second form)))))

(defn evaluate! [room party request authorize!]
  (let [{:keys [code from to]} (request-code request)]
    (binding [ec/*execution-context* (:ctx room)]
      ;; Registration is serialized, and only a ComponentRef is retained. The
      ;; component selects a separate SCI realization in descendant worlds.
      (let [ref (locking (:ctx room)
                  (or (get @sessions party)
                      (let [ref (sandbox/create-spindel-sci-world! (:ctx room))]
                        (swap! sessions assoc party ref) ref)))
            interpreter (sandbox/sci-context-in (:ctx room) ref)]
        (locking interpreter
          (when-not (= ref (get @sessions party))
            (throw (ex-info "SCI session was retired; retry in its replacement" {:type ::retired})))
          (authorize!)
          (let [namespace (when-not (= :file (:scope request)) (file-namespace (:text request)))
                program (if namespace (str "(in-ns '" namespace ")\n" code) code)
                result (binding [*print-length* 100 *print-level* 12]
                         (sandbox/eval-code interpreter program :execution-context (:ctx room)
                                            :timeout-ms 10000 :realize? true))]
            ;; A timed-out evaluator may still be unwinding. Never send the
            ;; next request into that same heap, including already queued work.
            (when (= "TimeoutException" (get-in result [:error :type]))
              (locking (:ctx room)
                (swap! sessions dissoc party)
                (sandbox/release-spindel-sci-world! (:ctx room) ref)))
            {:id (random-uuid) :target :human-sci :path (:path request)
             :source-revision (markdown/revision (:text request))
             :namespace (when namespace (str namespace))
             :from from :to to :code code
             :success (:success result) :value (when (:success result) (clipped (:value result)))
             :stdout (clipped (:stdout result)) :stderr (clipped (:stderr result))
             :error (when-not (:success result) (clipped (get-in result [:error :message])))
             :meter (:meter result)}))))))
