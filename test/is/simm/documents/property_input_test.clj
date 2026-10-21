(ns is.simm.documents.property-input-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.property-model :as properties]))

(deftest scalar-ui-input-is-closed-exact-and-inert
  (doseq [[type text expected]
          [[:db.type/string "" ""] [:db.type/string "  hello  " "  hello  "]
           [:db.type/string "#=(System/exit 1)" "#=(System/exit 1)"]
           [:db.type/boolean "false" false] [:db.type/boolean "true" true]
           [:db.type/long "0" 0] [:db.type/long "9007199254740993" 9007199254740993]
           [:db.type/long "9223372036854775807" Long/MAX_VALUE]
           [:db.type/long "-9223372036854775808" Long/MIN_VALUE]
           [:db.type/double "1.25e2" 125.0] [:db.type/double "-.5" -0.5]
           [:db.type/uuid "e9101cbe-a41c-47cb-8ad7-c643f4624a1f" "e9101cbe-a41c-47cb-8ad7-c643f4624a1f"]
           [:db.type/ref "e9101cbe-a41c-47cb-8ad7-c643f4624a1f" "e9101cbe-a41c-47cb-8ad7-c643f4624a1f"]
           [:db.type/instant "2026-10-21T19:00:00Z" "2026-10-21T19:00:00Z"]]]
    (is (= expected (properties/input-value type {:text text}))))
  (doseq [type [:db.type/string :db.type/boolean :db.type/long :db.type/ref]]
    (is (nil? (properties/input-value type {:null true}))))
  (doseq [[type input]
          [[:db.type/long {:text "9223372036854775808"}]
           [:db.type/long {:text "-9223372036854775809"}]
           [:db.type/long {:text "1.0"}] [:db.type/long {:text " 1"}]
           [:db.type/long {:text "1M"}] [:db.type/long {:text "(+ 1 2)"}]
           [:db.type/double {:text "NaN"}] [:db.type/double {:text "Infinity"}]
           [:db.type/double {:text "1e999"}] [:db.type/double {:text "0x1.0p1"}]
           [:db.type/boolean {:text "yes"}] [:db.type/boolean {:text "0"}]
           [:db.type/uuid {:text "1-1-1-1-1"}] [:db.type/ref {:text "some/path"}]
           [:db.type/instant {:text "tomorrow"}] [:db.type/bytes {:text "anything"}]
           [:db.type/string nil] [:db.type/string {:text nil}]
           [:db.type/string {:text "a" :type :db.type/string}]
           [:db.type/string {:null false}] [:db.type/string {:text "" :null true}]
           [:db.type/string {:text (apply str (repeat (inc (* 1024 1024)) "a"))}]]]
    (is (thrown? clojure.lang.ExceptionInfo (properties/input-value type input)))))
