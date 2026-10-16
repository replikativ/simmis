(ns is.simm.model.page-kind
  "What a property value of a KEYWORD attribute is, and which page kinds a
   person may set.

   `:S.Page/kind` is `:db.type/keyword` (schema `page-record-schema`). The
   property box edited every property as text, so it sent the string
   \"wiki/atom\" and datahike refused the transaction: the Kind of a page
   could not be saved at all. The editor now produces a keyword, and the
   server checks the value against the attribute's type before transacting.

   Some kinds are the SYSTEM's: `:bootstrap` marks the pages seeding installs
   (`model.seed/bootstrap-page-uuids`; the timeline hides them), and
   `:chat-summary` the summarizer's record pages (the nav hides them). A person
   may neither give a page one of those nor take one away — either would hide
   a page or unhide a record. The kinds a person sets are the wiki layout's
   (`:wiki/atom`, `:wiki/overview`, `:wiki/index`)."
  (:require [clojure.string :as str]))

(def attr :S.Page/kind)

(def user-kinds
  "The kinds a person can give a page, in the order the editor offers them."
  [:wiki/atom :wiki/overview :wiki/index])

(def system-kinds
  "Kinds only the system sets."
  #{:bootstrap :chat-summary})

(defn change-refusal
  "Why a person may not change a page's kind from `current` to `new` (nil
   clears it), or nil when they may."
  [current new]
  (cond
    (contains? system-kinds current)
    (str "This page's kind (" current ") is set by the system and cannot be changed here.")

    (and (some? new) (not (some #{new} user-kinds)))
    (str new " is not a page kind that can be set here; choose one of "
         (str/join ", " user-kinds) ".")))

(defn parse-keyword
  "The keyword a person typed into a keyword property: `wiki/atom` or
   `:wiki/atom` → `:wiki/atom`. Blank → nil (clear the value). Anything that
   does not read as one keyword → `::invalid`."
  [s]
  (let [t (str/trim (or s ""))
        t (if (str/starts-with? t ":") (subs t 1) t)]
    (cond
      (str/blank? t) nil
      (re-matches #"[A-Za-z*+!_?<>=-][\w*+!?<>=.-]*(/[A-Za-z*+!_?<>=-][\w*+!?<>=.-]*)?" t)
      (keyword t)
      :else ::invalid)))

(defn invalid?
  "Did `parse-keyword` find no keyword in what was typed?"
  [parsed]
  (= ::invalid parsed))

(def ^:private type-checks
  {:db.type/keyword keyword?
   :db.type/string  string?
   :db.type/boolean boolean?
   :db.type/long    integer?
   :db.type/double  number?
   :db.type/float   number?})

(defn value-fits?
  "Does `value` fit an attribute of `value-type`? Types not listed here are
   left to the transaction."
  [value-type value]
  (if-let [check (get type-checks value-type)]
    (boolean (check value))
    true))
