(ns is.simm.uis.web.desktop.foreign-lifecycle
  "Releasing what a spindel `foreign-node` acquired at mount.

   spindel calls `:on-unmount` with NIL, by design: the caller keeps its own
   handle to whatever `:on-mount` built (spindel `dom/foreign` docstring). The
   catch is WHICH `:on-unmount` runs. A re-render whose node is reconciled in
   place does not re-fire `:on-mount`, but it does replace the vnode, and the
   unmount fires the ref of the LAST committed vnode — a closure from a later
   render than the mount. A handle closed over in the render body (an
   `(atom nil)` in a `let`) is therefore a different, empty atom by the time
   the node goes. So was every handle simmis kept: the block editor's
   `destroy-tiptap-editor!` read the editor off the nil element, and the chat
   input's `editor-atom` was re-created on every render of the chat column.
   No TipTap editor was ever destroyed; each one, with its document-level
   listeners, its observer and its detached DOM, lived as long as the page.

   The handle has to be keyed by something every render of the SAME node
   computes alike, and two nodes never share. That is the node's place in the
   tree: its parent address and slot, which spindel's addressing derives
   deterministically (and from which it derives the node's own address), plus
   the node's `:key`. `owned` computes that identity in the render, files the
   release `mount!` returns under it, and runs it when any later render's
   `:on-unmount` fires for the same place."
  (:require [org.replikativ.spindel.dom.addressing :as addr]))

(defonce ^:private live
  ;; instance identity → the release thunk its mount returned
  (atom {}))

(defn- instance-id
  "Identity of the foreign node being built at this point of the render: the
   same on every render of that node, different for any other node."
  [k]
  [(addr/get-parent-addr) (addr/get-current-slot) k])

(defn- release! [id]
  (when-let [release (get @live id)]
    (swap! live dissoc id)
    (release)))

(defn owned
  "`:on-mount`/`:on-unmount` for a `foreign-node` keyed `k` whose mount
   acquires something that must be released. `(mount! el)` builds it and
   returns a thunk that releases it (or nil when it built nothing). Call this
   IN the node's position — inline in the `foreign-node` opts — so the identity
   is the node's own:

     (foreign-node
       (merge {:key k :class \"…\"}
              (owned k (fn [el] (let [ed (make-editor el)] #(.destroy ed))))))"
  [k mount!]
  (let [id (instance-id k)]
    {:on-mount (fn [el]
                 (when (contains? @live id)
                   ;; One identity, one live node. A second mount without an
                   ;; unmount between is a missed unmount or an identity
                   ;; collision — release the first rather than leak it, and say so.
                   #?(:cljs (js/console.error "[foreign-lifecycle] mounted twice without an unmount:"
                                              (pr-str k))
                      :clj nil)
                   (release! id))
                 (when-let [release (mount! el)]
                   (swap! live assoc id release)))
     :on-unmount (fn [_] (release! id))}))
