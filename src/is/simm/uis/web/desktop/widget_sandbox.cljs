(ns is.simm.uis.web.desktop.widget-sandbox
  "Client-side SCI sandbox for evaluating widget code from blocks.

   A 'widget' is a Clojure expression stored as :block/widget-code. The
   renderer evaluates it inside this curated SCI context and uses the
   resulting vnode in place of the block's text content.

   Vocabulary exposed to the widget:
     el/div, el/span, el/p, el/h1..h6, el/ul, el/ol, el/li, el/table,
     el/thead, el/tbody, el/tr, el/th, el/td, el/button, el/input,
     el/strong, el/em, el/code, el/pre, el/img, el/a
       — produce vnodes (without delta tracking; suitable for static views).

     dh/q, dh/pull, dh/entity
       — read-only datahike against the page's effective DB (passed in).

     kb/transact!, kb/upsert-block!, kb/set-attr!, kb/update-attr!,
     kb/retract!, kb/install-attr!
       — writes to the page's KB, each an RPC through the server's gate
         (`block-remote/widget-transact`, `…/widget-install-attr`), as the
         VIEWER. Data needs :write on the KB and is shown at once as a
         prediction on the KB's overlay; declaring an attribute needs :merge
         (the KB's owner) and names a codomain OBJECT, never a value type. A
         refused or failed write is rejected and reported, never left pending.

   What is NOT exposed:
     - No JS interop (no js/* ns, no js classes, no .-foo property access).
     - No DOM access (no foreign-node, no real DOM nodes).
     - No filesystem / network (SCI default).
     - No eval/load-file/load-string.

   Future work:
     - track/await for reactivity (need spindel SCI macro context port).
     - Per-eval timeout (interrupt-fn).

   Threat model: widget code authored by Vár or by another user; evaluated
   in the viewer's session. Must not exfiltrate, escape into JS, or freeze
   the tab beyond a budget."
  (:require [sci.core :as sci]
            [sci.ctx-store :as sci-ctx]
            [datahike.api :as d]
            [datahike.optimistic :as opt]
            [is.simm.uis.web.desktop.block-remote :as br]
            [is.simm.uis.web.desktop.optimistic-write :as ow]
            [is.simm.uis.web.desktop.remote :as rem]
            [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.core :as core]))

;; ---------------------------------------------------------------------------
;; Element function wrappers
;; ---------------------------------------------------------------------------
;;
;; The el/* macros require a spindel execution context for delta tracking.
;; For widget code we use simple-element instead — produces a plain vnode
;; without caching/delta machinery. This is fine for the V0 pure-render path:
;; the WIDGET BLOCK ITSELF is wrapped in spindel context by the block
;; renderer, so reactivity flows in via the parent (re-evaluate widget when
;; underlying db changes).

(def ^:private event-attr-keys
  "Attribute keys whose values are functions that escape into the host.
   We wrap them so each invocation runs under the widget's deadline budget."
  #{:on-click :on-change :on-input :on-submit :on-keydown :on-keyup :on-keypress
    :on-focus :on-blur :on-mouseenter :on-mouseleave :on-mousedown :on-mouseup
    :on-dblclick :on-contextmenu})

(defn- wrap-event-handlers
  "Wrap any event-handler fn in attrs so each invocation:
     1. runs under the widget's deadline budget (`with-deadline`),
     2. binds `sci.ctx-store/*ctx*` to the ctx held in `ctx-atom` (the
        handler is an SCI-defined closure; calling it from outside SCI
        without the ctx fails with 'No context found in sci.ctx-store/*ctx*').
   `ctx-atom` is filled in after SCI ctx construction (see `make-sandbox-ctx`)."
  [attrs with-deadline ctx-atom]
  (if (map? attrs)
    (reduce-kv (fn [m k v]
                 (if (and (contains? event-attr-keys k) (fn? v))
                   (assoc m k (fn [& args]
                                (with-deadline
                                  #(try
                                     (sci-ctx/with-ctx @ctx-atom
                                       (apply v args))
                                     (catch :default e
                                       ;; the person clicked something: a
                                       ;; failure is theirs to see, not the
                                       ;; console's
                                       (rem/report-error! "A widget action failed." e)
                                       nil)))))
                   (assoc m k v)))
               {} attrs)
    attrs))

(defn- mk-el [tag with-deadline ctx-atom]
  (fn [& args]
    (let [[attrs children] (el/parse-element-args args)
          attrs' (wrap-event-handlers attrs with-deadline ctx-atom)]
      (el/simple-element tag attrs' children))))

(defn- element-bindings-for
  "Build the el/* namespace map for an SCI ctx, with event-handler wrapping."
  [with-deadline ctx-atom]
  (into {}
        (map (fn [tag]
               [(symbol (name tag)) (mk-el tag with-deadline ctx-atom)])
             [:div :span :p :h1 :h2 :h3 :h4 :h5 :h6
              :ul :ol :li
              :table :thead :tbody :tr :th :td
              :button :input :textarea :select :option
              :strong :em :code :pre :small
              :img :a :br :hr :section :article :header :footer :nav :aside
              :label :form :details :summary])))

(defn- text [s]
  (core/make-text-vnode (str s)))

;; ---------------------------------------------------------------------------
;; SCI context setup
;; ---------------------------------------------------------------------------

(def ^:private default-timeout-ms
  "Wall-clock budget for a single widget evaluation. Beyond this, an interrupt
   fires inside SCI and aborts the eval. Keeps a runaway widget from freezing
   the tab. 250ms is generous for read-mostly UI rendering."
  250)

(defn- make-deadline-state
  "Returns a {:interrupt-fn :with-deadline}. The interrupt-fn is given to
   SCI; it fires only when a deadline is active (SCI's interrupt-fn runs at
   every user-fn entry, including invocations of closures that escape into
   the host as event handlers — without the active flag we'd time out a
   button click hours later).

   `with-deadline` wraps a thunk: arms the deadline, runs, disarms in a
   finally. For initial eval (and event handlers) we want a budget; outside
   those windows the interrupt is a no-op."
  [timeout-ms]
  (let [active-until (atom 0)]
    {:interrupt-fn (fn []
                     (let [until @active-until]
                       (when (and (pos? until) (> (.getTime (js/Date.)) until))
                         (throw (ex-info (str "widget timeout (" timeout-ms "ms)") {})))))
     :with-deadline (fn [thunk]
                      (reset! active-until (+ (.getTime (js/Date.)) timeout-ms))
                      (try (thunk)
                           (finally (reset! active-until 0))))}))

(defn- report-write-failure! [err]
  (rem/report-error! "The widget's write was not saved." err))

(defn- widget-writes
  "The `kb/*` namespace: writes to the KB at `db-scope`, through `overlay` (its
   datahike.optimistic overlay — the effective db the widget reads).

   They used to `d/transact!` the replica conn. That is the KabelWriter, which
   waits forever for a route simmis does not serve: no frame left the browser,
   nothing failed, and a reload brought the old value back. Now each is an RPC
   through the server's gate — see `block_remote.clj`, \"Widget writes\"."
  [db-scope overlay]
  (let [require-kb (fn [op]
                     (when-not (and db-scope overlay)
                       (throw (ex-info (str op " needs the page's KB on its trunk "
                                            "(no KB, or a fork is checked out)")
                                       {:op op}))))
        latest (fn [] (opt/db overlay))
        eid-of (fn [entity-ref]
                 (if (uuid? entity-ref) [:entity/uuid entity-ref] entity-ref))
        ;; DATA: shown at once as a prediction, made durable by the server,
        ;; rejected and reported if it is not.
        write! (fn [op tx-data]
                 (require-kb op)
                 (ow/predict-remote! overlay (vec tx-data)
                                     #(br/widget-transact-remote! db-scope (vec tx-data))
                                     {:on-failure report-write-failure!})
                 nil)
        next-block-order (fn [page-uuid]
                           (let [max-order (or (d/q '[:find (max ?o) .
                                                      :in $ ?puuid
                                                      :where
                                                      [?p :entity/uuid ?puuid]
                                                      [?b :block/parent ?p]
                                                      [?b :block/order ?o]]
                                                    (latest) page-uuid)
                                               "a")]
                             (str max-order "z")))]
    {'transact! (fn [tx-data] (write! 'kb/transact! tx-data))

     ;; Declare an attribute before the first write that uses it: KB DBs are
     ;; :schema-flexibility :write. Idempotent. Needs :merge on the KB (its
     ;; owner) — a `:db/ident` is append-only and global to the KB. The widget
     ;; names the CODOMAIN object; the server derives the value type from it
     ;; (doc/data-model.md, "Schema types"):
     ;;   (kb/install-attr! :todo/text "S/String" :one)
     ;;   (kb/install-attr! :todo/done? "S/Boolean" :one)
     ;;   (kb/install-attr! :tag/labels "S/String" :many)
     'install-attr! (fn [ident codomain cardinality]
                      (require-kb 'kb/install-attr!)
                      (when (keyword? codomain)
                        (throw (ex-info (str "kb/install-attr! takes a codomain object such as "
                                             "\"S/String\", not " codomain
                                             " — the server derives the value type")
                                        {:codomain codomain})))
                      (when-not (d/q '[:find ?e . :in $ ?id :where [?e :db/ident ?id]]
                                     (latest) ident)
                        ;; Not predicted: the replica cannot apply data for an
                        ;; attribute whose type it does not know yet. Queued in
                        ;; order with this KB's writes, so a write that follows
                        ;; it reaches the server after it.
                        (ow/send-in-order! overlay
                                           #(br/widget-install-attr-remote!
                                             db-scope ident codomain cardinality)
                                           {:on-failure report-write-failure!}))
                      ident)

     'upsert-block! (fn [page-uuid html-content]
                      (let [block-uuid (random-uuid)
                            now (js/Date.)]
                        (write! 'kb/upsert-block!
                                [{:entity/uuid block-uuid
                                  :entity/created-at now
                                  :entity/updated-at now
                                  :instance/of-role [:entity/name "S/Block"]
                                  :block/parent [:entity/uuid page-uuid]
                                  :block/order (next-block-order page-uuid)
                                  :block/content html-content}])
                        block-uuid))

     ;; Accept either a plain uuid or a lookup ref [:entity/uuid <uuid>] (or
     ;; any other unique-ident lookup), since both shapes are common in
     ;; Datalog code.
     'set-attr! (fn [entity-ref attr value]
                  (write! 'kb/set-attr! [(into {:db/id (eid-of entity-ref)} {attr value})]))

     ;; Read-modify-write against the EFFECTIVE db (the replica plus this
     ;; KB's pending predictions) — safe for rapid clicks: the second click
     ;; sees the first one's value before the server has answered.
     'update-attr! (fn [entity-ref attr f]
                     (require-kb 'kb/update-attr!)
                     (let [eid (eid-of entity-ref)
                           old-val (get (d/pull (latest) [attr] eid) attr)]
                       (write! 'kb/update-attr! [(into {:db/id eid} {attr (f old-val)})])))

     'retract! (fn [entity-ref]
                 (write! 'kb/retract! [[:db/retractEntity (eid-of entity-ref)]]))}))

(defn make-sandbox-ctx
  "Create a fresh SCI context for evaluating a widget.

   Args:
     db        — current Datahike DB value (for read queries via `dh/*`).
     kb        — `{:db-scope :overlay}` of the page's KB on its trunk, for the
                 `kb/*` writes; nil (or an absent overlay) when the page has
                 none, in which case a write throws a friendly error.
     timeout-ms — wall-clock budget for the eval.

   Each render gets a fresh ctx so atom state from prior renders cannot
   leak. Event handlers defined inside widget code (e.g. :on-click) close
   over the captured KB and can issue writes; each is an RPC authorized as
   the current viewer (not the agent that wrote the code).

   Returns {:ctx <sci-ctx> :with-deadline (fn [thunk] …)}.  Callers run
   `eval-string*` inside `(with-deadline …)` for the initial eval; event
   handlers in the produced vnode tree run under the same budget on each
   firing (see wrap-event-handlers)."
  [db {:keys [db-scope overlay]} timeout-ms]
  (let [{:keys [interrupt-fn with-deadline]} (make-deadline-state timeout-ms)
        ;; Atom holding this widget's SCI ctx — used by event handlers to
        ;; restore `sci.ctx-store/*ctx*` when fired outside the SCI eval.
        ;; Filled in immediately after `sci/init` returns.
        ctx-atom (atom nil)
        ctx (sci/init
              {:classes {:allow :all-cljs}
               :deny '[eval load-file load-string js* set!]
               :interrupt-fn interrupt-fn
               :namespaces
               {'el (assoc (element-bindings-for with-deadline ctx-atom) 'text text)

                'dh {'q (fn [query & args] (apply d/q query db args))
                     'pull (fn
                             ([selector eid] (d/pull db selector eid))
                             ([selector eid db'] (d/pull (or db' db) selector eid)))
                     'entity (fn
                               ([eid] (d/entity db eid))
                               ([db' eid] (d/entity (or db' db) eid)))
                     'datoms (fn [& args] (apply d/datoms db args))}

                'kb (widget-writes db-scope overlay)

                ;; The current db is exposed directly for advanced use. The
                ;; replica conn is NOT: it is a write handle past the gate.
                'user {'db db}}})]
    (reset! ctx-atom ctx)
    {:ctx ctx :with-deadline with-deadline}))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn eval-widget
  "Evaluate widget code against `db` (read) and `kb` — `{:db-scope :overlay}`,
   see `make-sandbox-ctx` — (writes).
   Returns:
     {:ok    <vnode>}     on success
     {:error <error-msg>} on parse or evaluation failure (including timeout).

   Never throws — intended to be called inside a render path. Optional
   `timeout-ms` overrides the default budget."
  ([code db] (eval-widget code db nil default-timeout-ms))
  ([code db kb] (eval-widget code db kb default-timeout-ms))
  ([code db kb timeout-ms]
   (try
     (let [{:keys [ctx with-deadline]} (make-sandbox-ctx db kb timeout-ms)
           result (with-deadline #(sci/eval-string* ctx (str code)))]
       {:ok result})
     (catch :default e
       {:error (or (.-message e) (str e))}))))
