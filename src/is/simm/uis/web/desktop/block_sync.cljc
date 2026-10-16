(ns is.simm.uis.web.desktop.block-sync
  "How an open block editor follows the replica.

   A wiki block's text lives in a TipTap editor, a foreign node: spindel
   renders its container and never its children, so a block whose content
   changed in the replica (another session's edit, synced) re-renders its item
   but the editor keeps showing what it was mounted with. The editor has to be
   TOLD. These are the rules for what it does when it is, kept free of the DOM
   so they can be tested on their own.

   What the editor compares against is the block's content in the EFFECTIVE
   db — the replica with this session's pending predictions on top
   (`optimistic-write`). A prediction therefore masks an older replica value
   for as long as it is pending, and once the replica holds the write it drops
   and the content does not change. So none of the rules below needs to know
   about predictions: an older replica value never reaches them while one is
   pending.

   An editor's sync state is a map:

     :applied   the content (as stored) the editor last took from the db
     :clean     the editor's own HTML right after it took `:applied` — what
                its text is when the user has not typed. TipTap normalises
                what it is given (plain `[[Page]]` text from an agent becomes
                a reference mark), so stored and shown content are not
                comparable directly; `:clean` is the shown form.
     :conflict  content the db holds that the editor did not take, because
                the user was editing (nil when none)

   Every block takes a change, except the one being edited — focused, or
   holding text the user typed and has not saved. That one keeps its text and
   records the change as a conflict, which the UI offers to load.")

(defn mounted
  "The sync state of an editor that just took `content`, showing `html`."
  [content html]
  {:applied content :clean html :conflict nil})

(defn dirty?
  "Has the user changed the editor's text since it last took the db's?"
  [state local]
  (not= local (:clean state)))

(defn incoming
  "What an editor does when the db's content for its block is `remote`, while
   it shows `local` and has focus or not. Returns `[action state']`:

     :none      nothing to do (the block did not change, or the change is
                what the editor already shows — e.g. this session's own save)
     :apply     set the editor to `remote`; then `(mounted remote html)`
     :conflict  keep the local text; `state'` records `remote` as a conflict"
  [state remote local focused?]
  (cond
    ;; the db agrees with the editor: our own write landed, or the other
    ;; session wrote what we have. Nothing to load, nothing to warn about.
    (= remote local)
    [:none (assoc state :applied remote :clean local :conflict nil)]

    ;; the block's content is what the editor last took: no change from
    ;; elsewhere (a conflict that was reverted resolves itself)
    (= remote (:applied state))
    [:none (assoc state :conflict nil)]

    (or focused? (dirty? state local))
    [:conflict (assoc state :conflict remote)]

    :else
    [:apply state]))

(defn leaving
  "What an editor does when it loses focus, showing `local`:

     :save   the user typed — save the local text (it wins over a conflict:
             they kept their text and saw the notice)
     :apply  nothing typed, but the db changed while they were in it — take
             that now (the conflict's content)
     :none   nothing typed, nothing changed"
  [state local]
  (cond
    (dirty? state local) :save
    (some? (:conflict state)) :apply
    :else :none))
