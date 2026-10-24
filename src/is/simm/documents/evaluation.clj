(ns is.simm.documents.evaluation
  "Human SCI sessions, owned by the room's execution world, not an agent.
   Saving source never calls this namespace. No room tools are injected."
  (:require [dvergr.sandbox :as sandbox]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.component :as component]
            [sci.core :as sci]
            [sci.ctx-store :as sci-store]
            [superficie.pipeline :as pipeline]
            [superficie.parse.reader :as sup-reader]
            [superficie.parse.expander :as expander]
            [superficie.emit.printer :as printer]))

(defn- copy-session-index [value _fork-id _directive] value)

(defn- session-index []
  (or (some-> (ec/get-state [:world/components ::sessions]) component/realization)
      (do (component/register! ::sessions (component/forkable {} copy-session-index)) {})))

(defn- update-session-index! [f & args]
  (ec/swap-state! [:world/components ::sessions]
                  (fn [index]
                    (component/forkable (apply f (component/realization index) args) copy-session-index))))

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

(defn- namespace-symbol [text]
  (when-not (and (string? text) (<= 1 (count text) 200)
                 (re-matches #"[A-Za-z_][A-Za-z0-9_.-]*" text))
    (throw (ex-info "Use a namespace such as user or research.market" {:type ::invalid})))
  (symbol text))

(defn validate-repl! [{:keys [text syntax namespace] :as request}]
  (when-not (and (= #{:text :syntax :namespace} (set (keys request)))
                 (#{:clojure :superficie} syntax) (string? text)
                 (<= 1 (count text) 16384) (not (str/blank? text)))
    (throw (ex-info "Choose Clojure or Superficie input (maximum 16K characters)" {:type ::invalid})))
  (markdown/source-bytes text)
  (namespace-symbol namespace)
  request)

(defn- sci-symbol [interpreter text]
  (when-let [v (sci/resolve interpreter (symbol text))]
    (let [{:keys [ns name]} (meta v)]
      (when (and ns name) (symbol (str ns) (str name))))))

(defn- generated-receipt [forms]
  (let [text (str/join "\n\n" forms)]
    {:generated-clojure (subs text 0 (min 65536 (count text)))
     :generated-truncated? (> (count text) 65536)}))

(defn repl-evaluate
  "Host adapter invoked INSIDE sandbox/eval-code's timeout and ctx-store scope.
   Resolves this invocation's SCI world; never captures a parent interpreter,
   invokes host eval, or registers user syntax in global Superficie registries."
  [{:keys [text syntax namespace] :as request}]
  (validate-repl! request)
  (let [interpreter (sci-store/get-ctx)
        generated (volatile! [])]
    (sci/binding [sci/ns (sci/find-ns interpreter 'user)]
      (try
        (let [ns-form (list 'ns (namespace-symbol namespace))
              _ (vswap! generated conj (printer/print-clj-string [ns-form]))
              _ (sci/eval-form interpreter ns-form)
              value
              (if (= :clojure syntax)
                (let [r (sci/reader text) eof (Object.)]
                  (vswap! generated conj text)
                  (loop [result nil]
                    (let [form (sci/parse-next interpreter r {:eof eof})]
                      (if (identical? eof form) result
                          (recur (sci/eval-form interpreter form))))))
                (let [opts {:resolve-sym #(sci-symbol interpreter %)
                            :resolve-keyword #(sci/parse-string interpreter %)}
                      scanned (-> {:source text :opts opts} pipeline/scan pipeline/group)
                      _ (when-let [error (first (:group-errors scanned))]
                          (throw (ex-info (:message error) {:type ::syntax :location (:loc error)})))
                      parser (sup-reader/streaming-parser (:tokens scanned) opts text)]
                  (loop [result nil]
                    (if (sup-reader/parser-done? parser) result
                        (let [raw (sup-reader/parse-next! parser)]
                          (if (sup-reader/discarded? raw) (recur result)
                              (let [form (first (expander/expand-forms [raw] opts))]
                                (vswap! generated conj (printer/print-clj-string [form]))
                                (recur (sci/eval-form interpreter form)))))))))
              ;; Realize/print while the selected interpreter is still bound.
              printed (binding [*print-length* 100 *print-level* 12] (sandbox/print-value value 12000))]
          (merge {:success true :value (:text printed) :value-truncated? (:truncated? printed)
                  :namespace (str @sci/ns)}
                 (generated-receipt @generated)))
        (catch Exception e
          (when (or (.isInterrupted (Thread/currentThread))
                    (some #(instance? InterruptedException %) (take-while some? (iterate ex-cause e))))
            (throw e))
          (merge {:success false :error (clipped (ex-message e))
                  :namespace (str @sci/ns) :partial? true}
                 (generated-receipt @generated)))))))

(defn- retire-session! [room party ref]
  (binding [ec/*execution-context* (:ctx room)]
    (locking (:ctx room)
      (when (= ref (get (session-index) party))
        (update-session-index! dissoc party)))
    (sandbox/release-spindel-sci-world! (:ctx room) ref)))

(defn- retire-or-retain! [room party ref lease]
  (try
    (retire-session! room party ref)
    (catch Throwable _
      ;; Keep the original native slot/key if exact-ref release needs recovery.
      (let [cleanup (preparation/retain-host-cleanup!
                     lease {:session [room party ref]}
                     (fn [[r p selected]] (retire-session! r p selected)))]
        ;; Room ownership alone cannot fence an application restart. Retain
        ;; BOTH original owners; failed index invalidation fences shared worlds.
        (when lifetime/*ticket*
          (lifetime/retain-operation-cleanup! (:join! cleanup)))))))

(defn- with-human-session! [room party authorize! evaluate control]
  (preparation/with-host-operation!
    room {:exclusive-key [::human-sci party]}
    (fn [lease]
      (binding [ec/*execution-context* (:ctx room)]
        ((:check! control))
        (authorize!)
        ;; Only stable ComponentRefs live in forkable state.
        (let [ref (locking (:ctx room)
                    (or (get (session-index) party)
                        (let [ref (sandbox/create-spindel-sci-world! (:ctx room))]
                          (update-session-index! assoc party ref) ref)))
              interpreter (sandbox/sci-context-in (:ctx room) ref)]
          ;; Distinct actual Rooms may share a world/interpreter. Keep this
          ;; gate through actual evaluation and cleanup, even after a receipt.
          (locking interpreter
            ((:check! control))
            (preparation/with-host-current! lease (constantly true))
            (when-not (= ref (get (session-index) party))
              (throw (ex-info "SCI session was retired" {:type ::retired})))
            (authorize!)
            (sci/merge-opts interpreter {:namespaces {'simmis.repl {'evaluate-source repl-evaluate}}})
            (try
              (binding [*print-length* 100 *print-level* 12] (evaluate interpreter))
              (finally
                ;; Room cancellation is independent of this invocation's
                ;; watchdog/caller. SCI can catch and clear its interrupt;
                ;; the actual lease state remains trusted stop evidence.
                (when (= :cancelled @(:state lease))
                  ((:cancel! control) :room-cancelled))
                ;; A cancelled admission waiter never touched SCI. Once
                ;; entered, retire only AFTER its body and awaited cleanup end.
                (when (and ((:aborted? control)) ((:entered? control)))
                  (retire-or-retain! room party ref lease))))))))))

(defn- execute! [room party program realize? authorize! with-owner]
  (let [handle (sandbox/start-evaluation!
                program
                {:timeout-ms 10000 :realize? realize?
                 :with-interpreter
                 (fn [evaluate control]
                   ;; Fresh ticket on the ACTUAL worker; the trusted adapter
                   ;; captures the original application, never its replacement.
                   (with-owner #(with-human-session! room party authorize! evaluate control)))})]
    (sandbox/await-evaluation! handle)))

(defn evaluate!
  ([room party request authorize!] (evaluate! room party request authorize! (fn [f] (f))))
  ([room party request authorize! with-owner]
   (let [{:keys [code from to]} (request-code request)
         namespace (when-not (= :file (:scope request)) (file-namespace (:text request)))
         program (if namespace (str "(in-ns '" namespace ")\n" code) code)
         result (execute! room party program true authorize! with-owner)]
     {:id (random-uuid) :target :human-sci :path (:path request)
      :source-revision (markdown/revision (:text request))
      :namespace (when namespace (str namespace))
      :from from :to to :code code
      :success (:success result) :value (when (:success result) (clipped (:value result)))
      :stdout (clipped (:stdout result)) :stderr (clipped (:stderr result))
      :error (when-not (:success result) (clipped (get-in result [:error :message])))
      :error-type (get-in result [:error :type]) :stop-reason (get-in result [:error :data :cause])
      :meter (:meter result)
      :invocation-id (:invocation-id result) :worker-finished? (:worker-finished? result)
      :evaluation-entered? (:evaluation-entered? result) :partial? (:partial? result)})))

(defn evaluate-repl!
  ([room party request authorize!] (evaluate-repl! room party request authorize! (fn [f] (f))))
  ([room party request authorize! with-owner]
   (validate-repl! request)
   (let [result (execute! room party (str "(simmis.repl/evaluate-source " (pr-str request) ")") false authorize! with-owner)]
     (merge {:id (random-uuid) :target :human-sci :input-source (:text request)
             :input-syntax (:syntax request) :namespace (:namespace request)
             :requested-namespace (:namespace request)
             :stdout (clipped (:stdout result)) :stderr (clipped (:stderr result))
             :meter (:meter result) :invocation-id (:invocation-id result)
             :worker-finished? (:worker-finished? result) :evaluation-entered? (:evaluation-entered? result)}
            (if (:success result) (:value result)
                {:success false :error (clipped (get-in result [:error :message]))
                 :error-type (get-in result [:error :type]) :stop-reason (get-in result [:error :data :cause])
                 :partial? (:partial? result)
                 :session-quarantined? (and (:evaluation-entered? result) (:partial? result))})))))
