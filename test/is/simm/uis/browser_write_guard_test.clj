(ns is.simm.uis.browser-write-guard-test
  "No browser code writes past the server's gate.

   A write the browser makes ITSELF — a `d/transact!` on a replica conn, a
   writer-backed `opt/transact!`, a konserve write into a synced store — has no
   authorized application half here. The replica's writer is datahike's
   KabelWriter. Before the shared RPC runtime, those calls waited for a route
   that never appeared. Wiki edits were lost that way from 2026-09-25
   (#49), and so were widget writes until they became RPCs. Nor should the
   server ever authorize that raw path: a raw transaction skips operation-level `can?`
   (`doc/authority.md`).

   So every browser write is an RPC — `block-remote` or a `defn-spin-remote` —
   and this test refuses the entry points that are not. It READS each
   ClojureScript file (`.cljs`, and the `:cljs` branch of every `.cljc`) and
   resolves every symbol through the file's own `:require` aliases, so a
   renamed alias, a fully qualified call or a `.cljc` reader conditional cannot
   slip past it the way a text grep could; comments and docstrings, which name
   these functions to explain why they are gone, are not code and do not
   count."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.reader :as tr]
            [clojure.tools.reader.reader-types :as rt]))

(def forbidden
  "Fully qualified vars a browser must not call, and why."
  (merge
   ;; datahike's own writes, on a replica conn: the KabelWriter
   (zipmap '[datahike.api/transact datahike.api/transact! datahike.api/load-entities
             datahike.api/create-database datahike.api/delete-database
             datahike.api/gc-storage]
           (repeat "an unauthorized raw datahike write on a client replica"))
   ;; a writer-backed overlay entry: the same writer as its durable half
   {'datahike.optimistic/transact! "an unauthorized writer-backed overlay write"}
   ;; the writer and its transport, called directly
   {'kabel.remote/invoke "use the authorized application RPC wrapper, not raw transport calls"
    'datahike.writer/create-writer "a client-side writer"
    'datahike.writer/transact! "a client-side writer"}
   ;; konserve writes into a store the server syncs to us
   (zipmap '[konserve.core/assoc konserve.core/assoc-in konserve.core/update
             konserve.core/update-in konserve.core/dissoc konserve.core/bassoc]
           (repeat "a konserve write into a client store"))))

(def forbidden-namespaces
  "Namespaces a browser must not require at all: their whole surface is the
   raw-write path."
  #{'datahike.kabel.writer})

(declare ns-aliases-of)

(defn- browser-files []
  (for [f (file-seq (io/file "src"))
        :let [n (.getName f)]
        :when (or (str/ends-with? n ".cljs") (str/ends-with? n ".cljc"))]
    f))

(defn- read-forms
  "Every top-level form of `f` as the ClojureScript compiler would see it."
  [f]
  (let [rdr (rt/indexing-push-back-reader (slurp f) 1 (str f))
        ;; `::alias/kw` needs the alias; the ns form never uses one
        aliases (atom {})]
    (binding [tr/*data-readers* {}
              tr/*default-data-reader-fn* (fn [_tag v] v)
              ;; tools.reader CALLS this resolver. An atom only happened to
              ;; work while no scanned form contained an auto-resolved alias.
              tr/*alias-map* #(get @aliases %)
              *ns* (create-ns 'is.simm.uis.browser-write-guard-test.scratch)]
      (loop [acc []]
        (let [form (tr/read {:eof ::eof :read-cond :allow :features #{:cljs}} rdr)]
          (cond
            (= ::eof form) acc
            :else
            (do (when (and (seq? form) (= 'ns (first form)))
                  (reset! aliases (ns-aliases-of form)))
                (recur (conj acc form)))))))))

(defn- ns-aliases-of
  "alias symbol -> namespace symbol, from an `ns` form's `:require`."
  [ns-form]
  (into {}
        (for [clause (rest ns-form)
              :when (and (seq? clause) (#{:require :require-macros} (first clause)))
              spec (rest clause)
              :when (vector? spec)
              :let [[lib & opts] spec
                    {:keys [as]} (apply hash-map (if (odd? (count opts)) (butlast opts) opts))]
              :when as]
          [as lib])))

(defn- required-namespaces [ns-form]
  (set (for [clause (rest ns-form)
             :when (and (seq? clause) (= :require (first clause)))
             spec (rest clause)]
         (if (vector? spec) (first spec) spec))))

(defn- offenders-in [f]
  (let [forms (read-forms f)
        ns-form (first (filter #(and (seq? %) (= 'ns (first %))) forms))
        aliases (if ns-form (ns-aliases-of ns-form) {})
        resolve-sym (fn [s]
                      (when-let [nsp (namespace s)]
                        (symbol (str (get aliases (symbol nsp) (symbol nsp))) (name s))))]
    (concat
     (for [n (some-> ns-form required-namespaces)
           :when (contains? forbidden-namespaces n)]
       {:file (str f) :var n :why "requires the raw-write namespace"})
     (for [form forms
           ;; a docstring or comment explaining the history is not a call
           x (tree-seq coll? seq form)
           :when (symbol? x)
           :let [v (resolve-sym x)]
           :when (contains? forbidden v)]
       {:file (str f) :var v :why (forbidden v)}))))

(deftest no-browser-code-writes-past-the-gate
  (let [files (browser-files)]
    (testing "the scan sees the browser code it is meant to guard"
      (is (some #(str/ends-with? (str %) "widget_sandbox.cljs") files))
      (is (some #(str/ends-with? (str %) "block_editor.cljs") files)))
    (let [offenders (mapcat offenders-in files)]
      (is (empty? offenders)
          (str "browser code writes past the server's gate — make it an RPC "
               "(block-remote or a defn-spin-remote), see optimistic-write:\n"
               (str/join "\n" (map #(str "  " (:file %) "  " (:var %) " — " (:why %))
                                   offenders)))))))

(deftest the-guard-catches-what-it-is-meant-to
  ;; The scan above passing proves nothing if it cannot see a call. Each of
  ;; these is the shape a regression would take.
  (doseq [[label src] {"d/transact! on a replica conn"
                       "(ns x (:require [datahike.api :as d])) (defn f [conn] (d/transact! conn []))"
                       "a writer-backed overlay write"
                       "(ns x (:require [datahike.optimistic :as o])) (defn f [ov] (o/transact! ov []))"
                       "the same call in a .cljc :cljs branch"
                       "(ns x (:require #?(:cljs [datahike.api :as dh]))) #?(:cljs (defn f [c] (dh/transact c [])))"
                       "fully qualified, no alias"
                       "(ns x) (defn f [c] (datahike.api/transact! c []))"
                       "an auto-resolved keyword before a guarded write"
                       "(ns x (:require [datahike.api :as d])) (defn f [c] (let [k ::d/example] (d/transact! c [k])))"
                       "a konserve write into a synced store"
                       "(ns x (:require [konserve.core :as k])) (defn f [s] (k/assoc s :db {}))"
                       "kabel.remote directly"
                       "(ns x (:require [kabel.remote :as kr])) (defn f [p] (kr/invoke p 1 'x {}))"
                       "requiring the KabelWriter"
                       "(ns x (:require [datahike.kabel.writer :as kw]))"}]
    (testing label
      (let [f (java.io.File/createTempFile "guard" ".cljc")]
        (try (spit f src)
             (is (seq (offenders-in f)))
             (finally (.delete f))))))
  (testing "a read, and the RPC a write now is, are not offenders"
    (let [f (java.io.File/createTempFile "guard" ".cljc")]
      (try (spit f (str "(ns x (:require [datahike.api :as d] [konserve.core :as k]))"
                        "(defn f [db s] (d/q '[:find ?e :where [?e :a]] db) (k/get s :db))"
                        ";; (d/transact! conn tx) — a comment is not a call\n"
                        "(defn g \"d/transact! used to be here\" [] nil)"))
           (is (empty? (offenders-in f)))
           (finally (.delete f)))))
  (testing "the cljc :clj branch is the server's, not the browser's"
    (let [f (java.io.File/createTempFile "guard" ".cljc")]
      (try (spit f "(ns x (:require [datahike.api :as d])) #?(:clj (defn f [c] (d/transact c [])))")
           (is (empty? (offenders-in f)))
           (finally (.delete f))))))
