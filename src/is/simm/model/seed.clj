(ns is.simm.model.seed
  "System seed data for the categorical schema.

   Creates the base system required for Simmis to function:
   - Category S (schema) with primitive types and system types
   - Category Comp (UI components)
   - System morphisms for Block, Page, Tag, Option properties
   - Getting Started page
   - Vár secretary AI user and chat room

   Does NOT include example/development data (see dev/is/simm/dev/seed.clj)."
  (:require [is.simm.model.schema :as schema]
            [is.simm.model.crud :as crud]
            [clojure.string :as str]
            [taoensso.telemere :as log]
            [datahike.api :as d]))

(defn- tx!
  "Transact `tx-data`, stamped at `at` when one is given.

   Every write in the seed path goes through here so that a store can be
   installed at the beginning of its NARRATIVE time rather than at the wall
   clock of whoever provisioned it. `at` nil is the live case and is exactly
   what datahike does on its own, so real tenants are unaffected.

   Why it matters: a seeded workspace back-dates its content (demo.scenario),
   which puts a cut EARLIER than an install stamped `(Date.)`. At such a cut the
   `S/Page` role entity does not exist, and any query naming it through a lookup
   ref — `[?e :instance/of-role [:entity/name \"S/Page\"]]` — throws `Nothing
   found for entity id` instead of returning nothing (measured 2026-07-27)."
  [conn at tx-data]
  (d/transact conn (cond-> {:tx-data (vec tx-data)}
                     at (assoc :tx-meta {:db/txInstant at}))))

;; ============================================================================
;; Category S (Schema Category)
;; ============================================================================

(def category-S
  "The schema category containing entity types, value types, and properties"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"
   :entity/name "S"
   :entity/created-at (java.util.Date.)})

;; ============================================================================
;; Primitive Value Types (Objects in S)
;; ============================================================================

(def object-String
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"
   :entity/name "S/String"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/primitive? true})

(def object-Number
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000011"
   :entity/name "S/Number"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/primitive? true})

(def object-Float
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000012"
   :entity/name "S/Float"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/primitive? true})

(def object-Boolean
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000013"
   :entity/name "S/Boolean"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/primitive? true})

(def object-Date
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000014"
   :entity/name "S/Date"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/primitive? true})

(def object-UUID
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000015"
   :entity/name "S/UUID"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/primitive? true})

;; Derived string types (with validation)
(def object-URL
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000016"
   :entity/name "S/URL"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/base-type [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]})

(def object-Email
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000017"
   :entity/name "S/Email"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/base-type [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]})

(def object-Phone
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000018"
   :entity/name "S/Phone"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/base-type [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]})

;; ============================================================================
;; Entity Types (Objects in S)
;; ============================================================================

(def object-Block
  "The fundamental entity type - everything is a Block"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"
   :entity/name "S/Block"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-Tag
  "Tag entity type for categorization"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000021"
   :entity/name "S/Tag"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-Page
  "Page entity type - top-level blocks with no parent"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000022"
   :entity/name "S/Page"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-EntityType
  "Entity type meta-type - for user-defined types (instance of itself)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"
   :entity/name "S/EntityType"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-PropertyType
  "Property type meta-type - for property type definitions"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000024"
   :entity/name "S/PropertyType"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-Option
  "Option entity type - for select/multi-select property options"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000025"
   :entity/name "S/Option"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-User
  "User entity type - represents a person with profile information"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"
   :entity/name "S/User"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-ChatRoom
  "ChatRoom entity type - a conversation space containing messages"
  {:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"
   :entity/name "S/ChatRoom"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-Message
  "Message entity type - a chat message (also a Block with content)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-00000000002b"
   :entity/name "S/Message"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-KBEvent
  "KB event entity type - records a knowledge base change in a room's timeline"
  {:entity/uuid #uuid "00000000-0000-0000-0000-00000000002c"
   :entity/name "S/KBEvent"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(def object-Person
  "Person entity type - the address-book projection of a party (human, agent,
   or contact) into the synced simmis store. This is the role @-mention
   autocomplete and the profile page resolve against, so humans (who live only
   in the un-synced system DB as :party/*) become locally queryable. A party
   may hold this role AND S/User concurrently (:instance/of-role is card-many).
   Also a usable wiki/CRM type: a page tagged S/Person carries the CRM property
   morphisms below and renders with the `users` icon (`:object/icon`)."
  {:entity/uuid #uuid "00000000-0000-0000-0000-00000000002d"
   :entity/name "S/Person"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/icon "users"                         ; Lucide icon; typed pages render with it
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

;; ============================================================================
;; Block Morphisms (Properties)
;; ============================================================================

(def morphism-Block-title
  "Block/title : Block → String"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000100"
   :entity/name "S/Block/title"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

(def morphism-Block-content
  "Block/content : Block → String"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000101"
   :entity/name "S/Block/content"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

(def morphism-Block-parent
  "Block/parent : Block → Block? (optional)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000102"
   :entity/name "S/Block/parent"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :relation})

(def morphism-Block-order
  "Block/order : Block → Number"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000103"
   :entity/name "S/Block/order"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000011"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :number})

(def morphism-Block-created-at
  "Block/created-at : Block → Date"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000104"
   :entity/name "S/Block/created-at"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000014"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :date})

(def morphism-Block-updated-at
  "Block/updated-at : Block → Date"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000105"
   :entity/name "S/Block/updated-at"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000014"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :date})

;; ============================================================================
;; Tag Morphisms
;; ============================================================================

(def morphism-Tag-color
  "Tag/color : Tag → String"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000110"
   :entity/name "S/Tag/color"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000021"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

;; ============================================================================
;; Page Morphisms
;; ============================================================================

(def morphism-Page-title
  "Page/title : Page → String"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000111"
   :entity/name "S/Page/title"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000022"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

(def morphism-Page-archived
  "Page/archived : Page → Boolean"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000112"
   :entity/name "S/Page/archived"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000022"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000013"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :checkbox})

(def morphism-Page-source
  "Page/source : Page → String (many) — the documents a wiki page's claims
   cite, as citation targets: `doc`, `doc#Section` or `dh://…` (the wiki
   layout pinned with dvergr's wiki/v4 checker; doc/data-model.md)."
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000113"
   :entity/name "S/Page/source"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000022"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :many
   :morphism/optional? true
   :morphism/property-type :text})

(def morphism-Page-entry-point
  "Page/entry-point : Page → String — where a reader of the source starts
   (a section, a timestamp, a file), for an atom or overview page."
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000114"
   :entity/name "S/Page/entry-point"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000022"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :text})

;; ============================================================================
;; Option Morphisms
;; ============================================================================

(def morphism-Option-name
  "Option/name : Option → String"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000120"
   :entity/name "S/Option/name"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000025"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

(def morphism-Option-color
  "Option/color : Option → String (optional)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000121"
   :entity/name "S/Option/color"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000025"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :text})

(def morphism-Option-for-property
  "Option/for-property : Option → Morphism (which property this option belongs to)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000122"
   :entity/name "S/Option/for-property"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000025"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000020"] ; Points to Block for now
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :relation})

;; ============================================================================
;; User Morphisms
;; ============================================================================

(def morphism-User-email
  "User/email : User → Email"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000130"
   :entity/name "S/User/email"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000017"] ; S/Email
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :email})

(def morphism-User-display-name
  "User/display-name : User → String"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000131"
   :entity/name "S/User/display-name"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"] ; S/String
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

(def morphism-User-avatar-url
  "User/avatar-url : User → URL (optional)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000132"
   :entity/name "S/User/avatar-url"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000016"] ; S/URL
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :url})

(def morphism-User-is-ai
  "User/is-ai : User → Boolean (flag for AI assistant users)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000133"
   :entity/name "S/User/is-ai"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000013"] ; S/Boolean
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :checkbox})

(def morphism-User-handle
  "User/handle : User → String (username handle, no spaces, for @mentions)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000134"
   :entity/name "S/User/handle"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"] ; S/String
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

;; ============================================================================
;; Person Morphisms (address-book projection)
;; ============================================================================

(def morphism-Person-id
  "Person/id : Person → UUID (synthetic canonical id; = :party/id for a
   projected party, a fresh uuid for an intake-created contact)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000160"
   :entity/name "S/Person/id"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002d"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000015"] ; S/UUID
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

(def morphism-Person-display-name
  "Person/display-name : Person → String"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000161"
   :entity/name "S/Person/display-name"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002d"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"] ; S/String
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

(def morphism-Person-handle
  "Person/handle : Person → String (username handle for @mentions)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000162"
   :entity/name "S/Person/handle"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002d"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"] ; S/String
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :text})

(def morphism-Person-avatar
  "Person/avatar : Person → String (emoji or url; optional)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000163"
   :entity/name "S/Person/avatar"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002d"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"] ; S/String
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :text})

(def morphism-Person-is-ai
  "Person/is-ai : Person → Boolean (true for agent parties)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000164"
   :entity/name "S/Person/is-ai"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002d"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000013"] ; S/Boolean
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :checkbox})

(def morphism-Person-party-type
  "Person/party-type : Person → String (human | agent | contact)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000165"
   :entity/name "S/Person/party-type"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002d"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"] ; S/String
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :text})

;; ---- CRM properties (wiki person pages / intake) ----------------------------
;; These make S/Person a usable contact type: editable in the wiki property UI
;; and settable by agents (Vár) via plain datahike transactions.

(defn- person-morphism
  "Terse S/Person/<prop> morphism builder for the CRM props below."
  [uuid prop dst-uuid prop-type]
  {:entity/uuid uuid
   :entity/name (str "S/Person/" prop)
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002d"]
   :morphism/dst [:entity/uuid dst-uuid]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type prop-type})

(def morphism-Person-company  (person-morphism #uuid "00000000-0000-0000-0000-000000000166" "company"  #uuid "00000000-0000-0000-0000-000000000010" :text))  ; S/String
(def morphism-Person-email    (person-morphism #uuid "00000000-0000-0000-0000-000000000167" "email"    #uuid "00000000-0000-0000-0000-000000000017" :email)) ; S/Email
(def morphism-Person-phone    (person-morphism #uuid "00000000-0000-0000-0000-000000000168" "phone"    #uuid "00000000-0000-0000-0000-000000000018" :text))  ; S/Phone
(def morphism-Person-linkedin (person-morphism #uuid "00000000-0000-0000-0000-000000000169" "linkedin" #uuid "00000000-0000-0000-0000-000000000016" :url))   ; S/URL
(def morphism-Person-title    (person-morphism #uuid "00000000-0000-0000-0000-00000000016a" "title"    #uuid "00000000-0000-0000-0000-000000000010" :text))  ; S/String
(def morphism-Person-source   (person-morphism #uuid "00000000-0000-0000-0000-00000000016b" "source"   #uuid "00000000-0000-0000-0000-000000000010" :text))  ; S/String (intake origin)
(def morphism-Person-status   (person-morphism #uuid "00000000-0000-0000-0000-00000000016c" "status"   #uuid "00000000-0000-0000-0000-000000000010" :text))  ; S/String (lead/customer/…)
(def morphism-Person-notes    (person-morphism #uuid "00000000-0000-0000-0000-00000000016d" "notes"    #uuid "00000000-0000-0000-0000-000000000010" :text))  ; S/String

;; ============================================================================
;; ChatRoom Morphisms
;; ============================================================================

(def morphism-ChatRoom-name
  "ChatRoom/name : ChatRoom → String"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000140"
   :entity/name "S/ChatRoom/name"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"] ; S/String
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :text})

(def morphism-ChatRoom-description
  "ChatRoom/description : ChatRoom → String (optional)"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000141"
   :entity/name "S/ChatRoom/description"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000010"] ; S/String
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type :text})

;; ============================================================================
;; Message Morphisms
;; ============================================================================

(def morphism-Message-author
  "Message/author : Message → User"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000150"
   :entity/name "S/Message/author"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002b"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"] ; S/User
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :relation})

(def morphism-Message-room
  "Message/room : Message → ChatRoom"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000151"
   :entity/name "S/Message/room"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002b"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"] ; S/ChatRoom
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :relation})

(def morphism-Message-sent-at
  "Message/sent-at : Message → Date"
  {:entity/uuid #uuid "00000000-0000-0000-0000-000000000152"
   :entity/name "S/Message/sent-at"
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002b"]
   :morphism/dst [:entity/uuid #uuid "00000000-0000-0000-0000-000000000014"] ; S/Date
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/property-type :date})

;; ============================================================================
;; Update Category S with Objects and Morphisms
;; ============================================================================

(def category-S-complete
  "Category S with all objects and morphisms"
  (assoc category-S
         :category/objects
         [[:entity/uuid (:entity/uuid object-String)]
          [:entity/uuid (:entity/uuid object-Number)]
          [:entity/uuid (:entity/uuid object-Float)]
          [:entity/uuid (:entity/uuid object-Boolean)]
          [:entity/uuid (:entity/uuid object-Date)]
          [:entity/uuid (:entity/uuid object-UUID)]
          [:entity/uuid (:entity/uuid object-URL)]
          [:entity/uuid (:entity/uuid object-Email)]
          [:entity/uuid (:entity/uuid object-Phone)]
          [:entity/uuid (:entity/uuid object-Block)]
          [:entity/uuid (:entity/uuid object-Tag)]]

         :category/morphisms
         [[:entity/uuid (:entity/uuid morphism-Block-title)]
          [:entity/uuid (:entity/uuid morphism-Block-content)]
          [:entity/uuid (:entity/uuid morphism-Block-parent)]
          [:entity/uuid (:entity/uuid morphism-Block-order)]
          [:entity/uuid (:entity/uuid morphism-Block-created-at)]
          [:entity/uuid (:entity/uuid morphism-Block-updated-at)]
          [:entity/uuid (:entity/uuid morphism-Tag-color)]]))

;; ============================================================================
;; Category Comp (UI Component Category)
;; ============================================================================

(def getting-started-page-uuid #uuid "30b7101d-db61-4266-a616-cb7bd6546784")

(def example-page-getting-started
  "Example page: Getting Started.

   `:S.Page/kind :bootstrap` — see `bootstrap-page-uuids`. It is part of what
   a store IS, not something anybody wrote in it."
  {:entity/uuid getting-started-page-uuid
   :entity/name "Getting Started"
   :entity/created-at (java.util.Date.)
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000022"] ; S/Page
   :S.Page/title "Getting Started"
   :S.Page/kind :bootstrap
   :S.Page/archived false})

(def getting-started-blocks
  "Blocks for the Getting Started page with instructions.
   Using fractional indexing for :block/order (lexicographically sortable strings)."
  [{:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000001"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a0"
    :block/content "<p>Welcome to <strong>Simmis</strong>! This is your wiki for organizing thoughts and knowledge.</p>"
    :block/collapsed false}
   {:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000002"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a1"
    :block/content "<p><strong>Creating Links</strong></p>"
    :block/collapsed false}
   {:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000003"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a2"
    :block/content "<p>Type <code>[[Page Name]]</code> to create a link to another page. If the page doesn't exist, clicking the link will create it!</p>"
    :block/collapsed false}
   {:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000004"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a3"
    :block/content "<p>Try it: [[My First Page]]</p>"
    :block/collapsed false}
   {:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000005"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a4"
    :block/content "<p><strong>Keyboard Shortcuts</strong></p>"
    :block/collapsed false}
   {:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000006"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a5"
    :block/content "<p><code>Enter</code> - Create new block below</p>"
    :block/collapsed false}
   {:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000007"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a6"
    :block/content "<p><code>Backspace</code> (empty block) - Delete block</p>"
    :block/collapsed false}
   {:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000008"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a7"
    :block/content "<p><code>Tab</code> - Indent block (make it a child)</p>"
    :block/collapsed false}
   {:entity/uuid #uuid "30b7101d-0001-0000-0000-000000000009"
    :block/parent [:entity/uuid getting-started-page-uuid]
    :block/order "a8"
    :block/content "<p><code>Shift+Tab</code> - Outdent block</p>"
    :block/collapsed false}])

;; ============================================================================
;; UI Functor Mappings (System Default)
;; ============================================================================

(def morphism-attribute-schemas
  "Datahike attribute schemas generated from morphisms.
   These are installed dynamically when the seed data is loaded."
  [(schema/morphism->attribute-schema morphism-Block-title "S/String")
   (schema/morphism->attribute-schema morphism-Block-content "S/String")
   (schema/morphism->attribute-schema morphism-Block-parent "S/Block")
   (schema/morphism->attribute-schema morphism-Block-order "S/Number")
   (schema/morphism->attribute-schema morphism-Block-created-at "S/Date")
   (schema/morphism->attribute-schema morphism-Block-updated-at "S/Date")
   (schema/morphism->attribute-schema morphism-Tag-color "S/String")
   (schema/morphism->attribute-schema morphism-Page-title "S/String")
   (schema/morphism->attribute-schema morphism-Page-archived "S/Boolean")
   (schema/morphism->attribute-schema morphism-Page-source "S/String")
   (schema/morphism->attribute-schema morphism-Page-entry-point "S/String")
   (schema/morphism->attribute-schema morphism-Option-name "S/String")
   (schema/morphism->attribute-schema morphism-Option-color "S/String")
   (schema/morphism->attribute-schema morphism-Option-for-property "S/Block")
   ;; User morphisms
   (schema/morphism->attribute-schema morphism-User-email "S/Email")
   (schema/morphism->attribute-schema morphism-User-display-name "S/String")
   (schema/morphism->attribute-schema morphism-User-avatar-url "S/URL")
   (schema/morphism->attribute-schema morphism-User-is-ai "S/Boolean")
   (schema/morphism->attribute-schema morphism-User-handle "S/String")
   ;; Person morphisms (address-book projection)
   (schema/morphism->attribute-schema morphism-Person-id "S/UUID")
   (schema/morphism->attribute-schema morphism-Person-display-name "S/String")
   (schema/morphism->attribute-schema morphism-Person-handle "S/String")
   (schema/morphism->attribute-schema morphism-Person-avatar "S/String")
   (schema/morphism->attribute-schema morphism-Person-is-ai "S/Boolean")
   (schema/morphism->attribute-schema morphism-Person-party-type "S/String")
   ;; ChatRoom morphisms
   (schema/morphism->attribute-schema morphism-ChatRoom-name "S/String")
   (schema/morphism->attribute-schema morphism-ChatRoom-description "S/String")
   ;; Message morphisms
   (schema/morphism->attribute-schema morphism-Message-author "S/User")
   (schema/morphism->attribute-schema morphism-Message-room "S/ChatRoom")
   (schema/morphism->attribute-schema morphism-Message-sent-at "S/Date")])

;; ============================================================================
;; All Seed Data
;; ============================================================================

(def all-seed-data
  "All seed entities to transact"
  (into
   [;; First: Base categories (without object/morphism refs)
    category-S

    ;; Second: S Objects (primitives)
    object-String
    object-Number
    object-Float
    object-Boolean
    object-Date
    object-UUID
    object-URL
    object-Email
    object-Phone

    ;; S Objects (entity types) - EntityType MUST come first since others reference it
    object-EntityType
    object-Block
    object-Tag
    object-Page
    object-PropertyType
    object-Option
    object-User
    object-Person
    object-ChatRoom
    object-Message
    object-KBEvent

    ;; Third: S Morphisms
    morphism-Block-title
    morphism-Block-content
    morphism-Block-parent
    morphism-Block-order
    morphism-Block-created-at
    morphism-Block-updated-at
    morphism-Tag-color
    morphism-Page-title
    morphism-Page-archived
    morphism-Page-source
    morphism-Page-entry-point
    morphism-Option-name
    morphism-Option-color
    morphism-Option-for-property
    ;; User morphisms
    morphism-User-email
    morphism-User-display-name
    morphism-User-avatar-url
    morphism-User-is-ai
    morphism-User-handle
    ;; Person morphisms (address-book projection)
    morphism-Person-id
    morphism-Person-display-name
    morphism-Person-handle
    morphism-Person-avatar
    morphism-Person-is-ai
    morphism-Person-party-type
    ;; ChatRoom morphisms
    morphism-ChatRoom-name
    morphism-ChatRoom-description
    ;; Message morphisms
    morphism-Message-author
    morphism-Message-room
    morphism-Message-sent-at


    ;; Sixth: Update categories with object/morphism refs
   {:entity/uuid (:entity/uuid category-S)
    :category/objects
    [[:entity/uuid (:entity/uuid object-String)]
     [:entity/uuid (:entity/uuid object-Number)]
     [:entity/uuid (:entity/uuid object-Float)]
     [:entity/uuid (:entity/uuid object-Boolean)]
     [:entity/uuid (:entity/uuid object-Date)]
     [:entity/uuid (:entity/uuid object-UUID)]
     [:entity/uuid (:entity/uuid object-URL)]
     [:entity/uuid (:entity/uuid object-Email)]
     [:entity/uuid (:entity/uuid object-Phone)]
     [:entity/uuid (:entity/uuid object-Block)]
     [:entity/uuid (:entity/uuid object-Tag)]
     [:entity/uuid (:entity/uuid object-Page)]
     [:entity/uuid (:entity/uuid object-EntityType)]
     [:entity/uuid (:entity/uuid object-PropertyType)]
     [:entity/uuid (:entity/uuid object-Option)]
     [:entity/uuid (:entity/uuid object-User)]
     [:entity/uuid (:entity/uuid object-Person)]
     [:entity/uuid (:entity/uuid object-ChatRoom)]
     [:entity/uuid (:entity/uuid object-Message)]
     [:entity/uuid (:entity/uuid object-KBEvent)]]
    :category/morphisms
    [[:entity/uuid (:entity/uuid morphism-Block-title)]
     [:entity/uuid (:entity/uuid morphism-Block-content)]
     [:entity/uuid (:entity/uuid morphism-Block-parent)]
     [:entity/uuid (:entity/uuid morphism-Block-order)]
     [:entity/uuid (:entity/uuid morphism-Block-created-at)]
     [:entity/uuid (:entity/uuid morphism-Block-updated-at)]
     [:entity/uuid (:entity/uuid morphism-Tag-color)]
     [:entity/uuid (:entity/uuid morphism-Page-title)]
     [:entity/uuid (:entity/uuid morphism-Page-archived)]
     [:entity/uuid (:entity/uuid morphism-Page-source)]
     [:entity/uuid (:entity/uuid morphism-Page-entry-point)]
     [:entity/uuid (:entity/uuid morphism-Option-name)]
     [:entity/uuid (:entity/uuid morphism-Option-color)]
     [:entity/uuid (:entity/uuid morphism-Option-for-property)]
     ;; User morphisms
     [:entity/uuid (:entity/uuid morphism-User-email)]
     [:entity/uuid (:entity/uuid morphism-User-display-name)]
     [:entity/uuid (:entity/uuid morphism-User-avatar-url)]
     [:entity/uuid (:entity/uuid morphism-User-is-ai)]
     [:entity/uuid (:entity/uuid morphism-User-handle)]
     ;; Person morphisms (address-book projection)
     [:entity/uuid (:entity/uuid morphism-Person-id)]
     [:entity/uuid (:entity/uuid morphism-Person-display-name)]
     [:entity/uuid (:entity/uuid morphism-Person-handle)]
     [:entity/uuid (:entity/uuid morphism-Person-avatar)]
     [:entity/uuid (:entity/uuid morphism-Person-is-ai)]
     [:entity/uuid (:entity/uuid morphism-Person-party-type)]
     ;; ChatRoom morphisms
     [:entity/uuid (:entity/uuid morphism-ChatRoom-name)]
     [:entity/uuid (:entity/uuid morphism-ChatRoom-description)]
     ;; Message morphisms
     [:entity/uuid (:entity/uuid morphism-Message-author)]
     [:entity/uuid (:entity/uuid morphism-Message-room)]
     [:entity/uuid (:entity/uuid morphism-Message-sent-at)]]}]
   ;; The Getting Started page and its blocks are deliberately NOT here. They
   ;; are CONTENT, and this vector is transacted unconditionally on every boot
   ;; — see `seed-getting-started-page!` for what that did to them.
   ))

(declare seed-var-agent! seed-skill-page! mark-bootstrap-pages!)

(def person-morphisms
  "All S/Person morphisms: the address-book projection props first, then the
   CRM/wiki props. Order-independent; used for both seed data and the migration."
  [morphism-Person-id morphism-Person-display-name morphism-Person-handle
   morphism-Person-avatar morphism-Person-is-ai morphism-Person-party-type
   morphism-Person-company morphism-Person-email morphism-Person-phone
   morphism-Person-linkedin morphism-Person-title morphism-Person-source
   morphism-Person-status morphism-Person-notes])

(def person-attribute-schemas
  "Datahike attribute schemas (:S.Person/*) generated from the morphisms — the
   dst object name drives the value type (see schema/codomain->db-type)."
  (mapv (fn [[m dst]] (schema/morphism->attribute-schema m dst))
        [[morphism-Person-id "S/UUID"]     [morphism-Person-display-name "S/String"]
         [morphism-Person-handle "S/String"] [morphism-Person-avatar "S/String"]
         [morphism-Person-is-ai "S/Boolean"] [morphism-Person-party-type "S/String"]
         [morphism-Person-company "S/String"] [morphism-Person-email "S/Email"]
         [morphism-Person-phone "S/Phone"]  [morphism-Person-linkedin "S/URL"]
         [morphism-Person-title "S/String"] [morphism-Person-source "S/String"]
         [morphism-Person-status "S/String"] [morphism-Person-notes "S/String"]]))

(def ^:private object-icon-schema
  "The `:object/icon` attribute — a Lucide icon name on a type object, so a page
   tagged with that type can render the type's icon instead of the default."
  {:db/ident :object/icon :db/valueType :db.type/string :db/cardinality :db.cardinality/one})

;; ============================================================================
;; S/Task — a work item as CONTENT
;; ============================================================================

(def object-Task
  "Task entity type — a work item that is a PAGE, so it lives in the wiki graph
   rather than beside it.

   Deliberately NOT dvergr's `:task/*` (see doc/archive/navigation-redesign-plan.md,
   \"Resolved: S/Task vs dvergr :task/*\"). Those rows are a DISPATCH: one actor
   asking another to do something, with an accept/complete/ignore lifecycle.
   This is the durable work item, and being a page is the whole point — it gets
   blocks, [[links]] and backlinks, fulltext, the page renderer, the kb/* agent
   vocabulary and semantic diffs for free, and it can be part of a ForkSet, so
   an agent can PROPOSE a set of tasks for a human to accept. A flat row can do
   none of that without bespoke machinery for each."
  {:entity/uuid #uuid "00000000-0000-0000-0000-00000000002e"
   :entity/name "S/Task"
   :object/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :object/icon "circle-check"
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000023"]})

(defn- task-morphism
  "Terse S/Task/<prop> morphism builder. All optional: a page becomes a task by
   being tagged S/Task, and a task with nothing but a title is a legitimate
   task."
  [uuid prop dst-uuid prop-type]
  {:entity/uuid uuid
   :entity/name (str "S/Task/" prop)
   :morphism/src [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002e"]
   :morphism/dst [:entity/uuid dst-uuid]
   :morphism/of-category [:entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]
   :morphism/cardinality :one
   :morphism/optional? true
   :morphism/property-type prop-type})

;; Status and priority are STRINGS, not keywords: they cross the wire to the
;; client and get typed into a property field by hand. `is.simm.ops.tasks` owns
;; the vocabulary and normalises on write; the database stays permissive so an
;; agent inventing "waiting-on-tenant" writes something readable rather than
;; failing a transaction.
(def morphism-Task-status   (task-morphism #uuid "00000000-0000-0000-0000-00000000016e" "status"   #uuid "00000000-0000-0000-0000-000000000010" :text))   ; S/String
(def morphism-Task-priority (task-morphism #uuid "00000000-0000-0000-0000-00000000016f" "priority" #uuid "00000000-0000-0000-0000-000000000010" :text))   ; S/String
(def morphism-Task-due      (task-morphism #uuid "00000000-0000-0000-0000-000000000170" "due"      #uuid "00000000-0000-0000-0000-000000000014" :date))   ; S/Date
(def morphism-Task-done-at  (task-morphism #uuid "00000000-0000-0000-0000-000000000171" "done-at"  #uuid "00000000-0000-0000-0000-000000000014" :date))   ; S/Date
;; Assignee is a party UUID by VALUE, not a ref: the party lives in the system
;; DB and this page lives in a KB store. A ref would be an eid pointing into
;; another database — the mistake that produced the eid-collision incident (see
;; ops/semantic_diff.clj). Same reasoning as :S.Message/party-mentions.
(def morphism-Task-assignee (task-morphism #uuid "00000000-0000-0000-0000-000000000172" "assignee" #uuid "00000000-0000-0000-0000-000000000015" :uuid))  ; S/UUID
;; The ForkSet this task came out of, when a proposal was accepted into tasks.
;; Also by value — proposals live in the system DB.
(def morphism-Task-forkset  (task-morphism #uuid "00000000-0000-0000-0000-000000000173" "forkset"  #uuid "00000000-0000-0000-0000-000000000015" :uuid))  ; S/UUID

(def task-morphisms
  [morphism-Task-status morphism-Task-priority morphism-Task-due
   morphism-Task-done-at morphism-Task-assignee morphism-Task-forkset])

(def task-attribute-schemas
  "Datahike attribute schemas (:S.Task/*) generated from the morphisms."
  (mapv (fn [[m dst]] (schema/morphism->attribute-schema m dst))
        [[morphism-Task-status "S/String"]   [morphism-Task-priority "S/String"]
         [morphism-Task-due "S/Date"]        [morphism-Task-done-at "S/Date"]
         [morphism-Task-assignee "S/UUID"]   [morphism-Task-forkset "S/UUID"]]))

(defn seed-getting-started-page!
  "Seed the example Getting Started page. Idempotent — skips if it exists.

   GUARDED, unlike the schemas and category rows around it, and the distinction
   is the point: `ensure-seed-data!` is unconditional so that a morphism added
   to `morphism-attribute-schemas` reaches stores that already exist. That
   argument covers SCHEMA. It does not cover CONTENT, and applying it to content
   had two consequences, both measured on 2026-07-27:

     1. Every boot advanced the transaction of every seeded block, because
        re-transacting an identical map re-stamps its datoms. So the Timelines
        audit view reported `Getting Started changed` in every wiki at every
        restart — noise indistinguishable from a real edit, in the one view
        whose job is accounting for change.

     2. `:block/collapsed false` accumulated a NEW datom per boot rather than
        deduplicating like `:block/content` does — reproduced in a bare
        in-memory datahike, 3 identical transactions giving 3 datoms for the
        boolean and 1 for the string. Whatever the cause upstream, the effect
        here was that collapsing a block did not survive a server restart: the
        seed silently reset it.

   Both disappear once content is written once and left alone."
  ([conn] (seed-getting-started-page! conn nil))
  ([conn at]
   (when-not (seq (d/q '[:find ?e :in $ ?u :where [?e :entity/uuid ?u]]
                       @conn getting-started-page-uuid))
     (tx! conn at [example-page-getting-started])
     (tx! conn at getting-started-blocks))))

(defn definition-transactions
  "Categorical vocabulary only, in dependency order. Contains no pages,
   agents, chat rooms, example files or accounting accounts. Shared by ordinary
   provisioning and the explicit preserving knowledge upgrade."
  []
  [(vec (concat morphism-attribute-schemas person-attribute-schemas task-attribute-schemas))
   all-seed-data
   (into [object-Person] person-morphisms)
   (into [object-Task] task-morphisms)
   [{:entity/uuid (:entity/uuid category-S)
     :category/objects [[:entity/uuid (:entity/uuid object-Person)]
                        [:entity/uuid (:entity/uuid object-Task)]]
     :category/morphisms (mapv #(vector :entity/uuid (:entity/uuid %))
                               (concat person-morphisms task-morphisms))}]])

(defn ensure-seed-data!
  "Install the categorical seed into `conn`. Idempotent, and unconditionally so.

   The attribute schemas and the seed entities are now transacted on EVERY call,
   not only when category S is absent. That guard is why
   `ensure-person-schema!` and `ensure-task-schema!` had to exist: a morphism
   added to `morphism-attribute-schemas` reached fresh stores only, so every new
   type needed a hand-written migration beside it — and the next one would have
   needed another. Both are now deleted; adding a morphism to the vectors below
   is enough.

   It is safe because every upsert is keyed on `:db/ident` or `:entity/uuid`, and
   because `store/ensure!` runs `install!` once per store per process, so this is
   a few hundred idempotent datoms at boot rather than per request — the same
   deal `install!` already accepts for `full-schema`.

   ORDER still matters: the attribute schemas precede the entities that use
   them, because `:schema-flexibility :write` rejects an undeclared attribute.

   `at` stamps every transaction below with `:db/txInstant` — the store's
   narrative birth rather than the moment it was provisioned. See `tx!` for the
   query that throws without it. On the RE-install path (an existing store
   picking up a new morphism) the caller passes nothing, because that write
   really is happening now."
  ([conn] (ensure-seed-data! conn nil))
  ([conn at]
   (let [seed-exists? (seq (d/q '[:find ?e
                                  :where
                                  [?e :entity/uuid #uuid "00000000-0000-0000-0000-000000000001"]]
                                @conn))]
     (doseq [tx (definition-transactions)] (tx! conn at tx))
     seed-exists?)
   ;; Content, each guarded on its own existence — see
   ;; `seed-getting-started-page!` for why content must not ride the
   ;; unconditional path the schemas above take.
   (seed-getting-started-page! conn at)
   ;; Always ensure Vár agent is seeded (idempotent)
   (seed-var-agent! conn at)
   ;; Always ensure SKILL page is seeded (idempotent)
   (seed-skill-page! conn at)
   ;; Last, because it upserts ONTO the two pages above: stores created before
   ;; the marker existed pick it up here. See `mark-bootstrap-pages!`.
   (mark-bootstrap-pages! conn)))

;; ============================================================================
;; Chat Seed Data (for development/testing)
;; ============================================================================

;; Vár secretary agent UUIDs - stable for referencing
(def var-user-uuid #uuid "00000000-0000-0000-0000-000000000300")
(def var-room-uuid #uuid "00000000-0000-0000-0000-000000000301")
(def var-participant-uuid #uuid "00000000-0000-0000-0000-000000000302")
(def dev-user-participant-uuid #uuid "00000000-0000-0000-0000-000000000303")

;; User entity UUIDs - these are stable so we can reference them
(def user-uuid-you #uuid "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
(defn generate-chat-room
  "Generate a chat room entity using the categorical schema.
   ChatRooms are entities with S/ChatRoom type."
  [room-uuid room-name & [description]]
  (cond-> {:entity/uuid room-uuid
           :entity/name room-name
           :entity/created-at (java.util.Date.)
           :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"] ; S/ChatRoom
           :S.ChatRoom/name room-name}
    description (assoc :S.ChatRoom/description description)))

(def skill-page-uuid #uuid "00000000-0000-0000-0000-000000000400")

(def skill-page
  "The Vár SKILL page — documents capabilities and current focus.
   Users can edit this in the wiki to customize Vár's behavior.

   `:S.Page/kind :bootstrap` — see `bootstrap-page-uuids`."
  {:entity/uuid skill-page-uuid
   :entity/name "SKILL"
   :entity/created-at (java.util.Date.)
   :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000022"] ; S/Page
   :S.Page/title "SKILL"
   :S.Page/kind :bootstrap
   :S.Page/archived false})

(def skill-page-blocks
  "Initial blocks for the SKILL page documenting Vár's REPL namespaces."
  [{:entity/uuid #uuid "00000000-0000-0000-0000-000000000401"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a0"
    :block/content "<p>This page documents Vár's capabilities and current focus. Edit it to customize Vár's behavior — changes take effect on next session start.</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000402"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a1"
    :block/content "<p><strong>Available REPL Namespaces</strong></p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000403"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a2"
    :block/content "<p><code>(wiki/pages)</code> — list all wiki page titles</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000404"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a3"
    :block/content "<p><code>(wiki/read-page \"Title\")</code> — read page blocks, returns {:title ... :blocks [...]}</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000405"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a4"
    :block/content "<p><code>(wiki/search \"query\")</code> — search page titles by regex pattern</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000406"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a5"
    :block/content "<p><code>(kb/ensure-page! \"Title\")</code> — create page if not exists, returns uuid</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000407"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a6"
    :block/content "<p><code>(kb/upsert-block! page-uuid \"&lt;p&gt;content&lt;/p&gt;\")</code> — append block</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000408"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a7"
    :block/content "<p><code>(kb/upsert-viz-block! page-uuid spec-map \"caption\")</code> — append Vega-Lite chart</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000409"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a8"
    :block/content "<p><code>(kb/retract-block! block-uuid)</code> — delete a block</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000410"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "a9"
    :block/content "<p><code>(dh/q '[:find ... :where ...])</code> — raw Datalog query</p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000411"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "b0"
    :block/content "<p><strong>Current Focus</strong></p>"}
   {:entity/uuid #uuid "00000000-0000-0000-0000-000000000412"
    :block/parent [:entity/uuid skill-page-uuid]
    :block/order "b1"
    :block/content "<p>Help users organize their work and build knowledge bases. When a user describes a project or dataset, offer to create structured wiki pages with organized blocks and visualizations where appropriate.</p>"}])

(defn seed-skill-page!
  "Seed the Vár SKILL page. Idempotent — skips if page already exists."
  ([conn] (seed-skill-page! conn nil))
  ([conn at]
   (let [db @conn
         skill-exists? (seq (d/q '[:find ?e
                                   :where
                                   [?e :entity/uuid #uuid "00000000-0000-0000-0000-000000000400"]]
                                 db))]
     (when-not skill-exists?
       (tx! conn at [skill-page])
       (tx! conn at skill-page-blocks)))))

;; ============================================================================
;; Bootstrap pages are infrastructure, not narrative
;; ============================================================================

(def bootstrap-page-uuids
  "The pages `install!` puts in EVERY store, whoever the store belongs to.

   They are part of what a simmis store IS — the same status as its schema and
   its category-S rows — and nobody in the workspace wrote them. The Timelines
   difference panel is an account of what CHANGED, and a fresh wiki's two seed
   pages landing there next to the day's real work is the panel reporting on the
   act of provisioning itself.

   Marked in the DATA (`:S.Page/kind :bootstrap`, the attribute whose documented
   job is `a generated RECORD rather than a page a human wrote`) rather than
   filtered by title in the view. A title filter would hide any page a user
   happened to call `SKILL`, and would miss these the moment a store is seeded
   in another language.

   Note what this deliberately does NOT do: it does not hide the pages. They are
   in the wiki, in search, in the sidebar and in `[[Getting Started]]` links —
   `views/nav` filters only `:chat-summary`. The one place they are excluded is
   the audit panel, because that panel answers `what changed`."
  [getting-started-page-uuid skill-page-uuid])

(defn mark-bootstrap-pages!
  "Backfill `:S.Page/kind :bootstrap` onto stores seeded before the marker
   existed.

   New stores get it in the page's own creation transaction (it is on the page
   maps), so this writes nothing for them. It is guarded rather than transacted
   unconditionally: an unconditional upsert would be a no-op datom-wise but
   still mint a transaction in every store on every boot, and this file already
   carries the scar of content re-transacted on the unconditional path (see
   `seed-getting-started-page!`).

   The guard asks which pages ARE marked rather than `missing?`. Measured
   2026-07-27: after retracting a datom that was asserted in its entity's own
   creation transaction, `(missing? $ ?e :S.Page/kind)` still reports the
   attribute as present while a plain `[?e :S.Page/kind ?k]` correctly finds
   nothing — so a guard built on `missing?` would refuse to repair the very
   stores it exists for. The positive query is also the cheaper one: it reads an
   indexed attribute that at most a handful of entities in any store carry.

   Not stamped with `at`: this is a repair happening now, and dating it into the
   store's narrative past would be inventing the moment the workspace learned
   something about itself."
  [conn]
  (let [db @conn
        marked (into #{} (d/q '[:find [?u ...]
                                :where [?e :S.Page/kind :bootstrap]
                                [?e :entity/uuid ?u]]
                              db))
        needs (into []
                    (comp (remove marked)
                          (map (fn [u]
                                 (d/q '[:find ?e . :in $ ?u
                                        :where [?e :entity/uuid ?u]]
                                      db u)))
                          (filter some?)
                          (map (fn [e] {:db/id e :S.Page/kind :bootstrap})))
                    bootstrap-page-uuids)]
    (when (seq needs)
      (log/log! {:level :info :id ::bootstrap-pages-marked
                 :msg "Marked seed pages as workspace bootstrap"
                 :data {:pages (count needs)}})
      (d/transact conn needs))))

;; ============================================================================
;; Vár Secretary Agent Seed Data
;; ============================================================================

(defn seed-var-agent!
  "Seed the Vár S/User entity used for message authorship.
   The room and agent config are created per-user via ensure-personal-ai-room!.
   Idempotent — skips if Vár user already exists."
  ([conn] (seed-var-agent! conn nil))
  ([conn at]
   (let [db @conn
         var-exists? (seq (d/q '[:find ?e
                                 :where
                                 [?e :entity/uuid #uuid "00000000-0000-0000-0000-000000000300"]]
                               db))]
     (when-not var-exists?
       (log/log! {:level :info :id ::seed-var :msg "Seeding Vár S/User entity"})
       (tx! conn at
            [{:entity/uuid var-user-uuid
              :entity/name "Vár (AI)"
              :entity/created-at (or at (java.util.Date.))
              :instance/of-role [:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"] ; S/User
              :S.User/display-name "Vár"
              :S.User/handle "var"
              :S.User/is-ai true}])))))

(comment
  ;; To load seed data:
  (require '[datahike.api :as d])
  (require '[is.simm.model.db :as db])

  ;; First install dynamic attribute schemas
  (d/transact (db/get-conn) morphism-attribute-schemas)

  ;; Then load seed data
  (d/transact (db/get-conn) all-seed-data)

  ;; Verify categories exist
  (d/q '[:find [(pull ?c [:entity/name :category/objects :category/morphisms]) ...]
         :where [?c :category/objects _]]
       @(db/get-conn))

  )
