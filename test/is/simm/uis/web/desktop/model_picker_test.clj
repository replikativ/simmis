(ns is.simm.uis.web.desktop.model-picker-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.views.model-picker :as picker]))

(def unavailable-row
  {:value "gpt-*-luna"
   :available? false
   :availability :needs-credential
   :availability-label "Credential required"
   :availability-explanation
   "Set OPENAI_API_KEY in the server environment, then restart simmis."})

(deftest unavailable-rows-have-focusable-semantic-disabled-state
  (let [attrs (picker/semantic-attrs unavailable-row)]
    (is (= "button" (:role attrs)))
    (is (= 0 (:tabindex attrs)))
    (is (= "true" (:aria-disabled attrs)))
    (is (= (picker/explanation-id (:value unavailable-row))
           (:aria-describedby attrs))))

  (let [attrs (picker/semantic-attrs
               (assoc unavailable-row :available? true))]
    (is (= "false" (:aria-disabled attrs)))
    (is (nil? (:aria-describedby attrs)))))

(deftest pointer-and-keyboard-activation-are-both-fail-closed
  (let [selected (atom [])
        unavailable (picker/option-attrs unavailable-row #(swap! selected conj %))
        available (picker/option-attrs (assoc unavailable-row :available? true)
                                       #(swap! selected conj %))]
    (testing "disabled rows reject mouse, Enter, and Space"
      ((:on-click unavailable) {})
      ((:on-key-down unavailable) {:key "Enter"})
      ((:on-key-down unavailable) {:key " "})
      (is (= [] @selected)))
    (testing "available rows accept pointer and activation keys"
      ((:on-click available) {})
      ((:on-key-down available) {:key "Enter"})
      ((:on-key-down available) {:key " "})
      ((:on-key-down available) {:key "ArrowDown"})
      (is (= 3 (count @selected)))
      (testing "and hand the handler the whole row, target included"
        (is (every? #(= "gpt-*-luna" (:value %)) @selected))))))

(def choices
  [{:value "gpt-*-sol" :label "GPT Sol (Latest)" :kind :family :available? true}
   {:value "gpt-*-terra" :label "GPT Terra (Latest)" :kind :family :available? true}
   {:value "gpt-5.4-mini" :label "GPT mini 5.4" :kind :version :available? true}])

(defn- resolution
  "One agent's server-side resolution, pinned to `family` with :auto? true."
  [family]
  {:configured? true
   :auto? true
   :family family
   :model (str family "-latest")
   :inheritance-choice {:value "inherit"
                        :label "Inherit"
                        :kind :inheritance
                        :available? true}})

(deftest rows-of-two-agents-are-never-equal-items
  (testing "the invariant ifor-each needs: same model value, different item"
    (let [lun (picker/agent-model-rows "room-a" "lun-id" "Lun"
                                       (resolution "gpt-*-sol") choices)
          vaar (picker/agent-model-rows "room-a" "var-id" "Vár"
                                        (resolution "gpt-*-terra") choices)
          by-value (fn [rows] (into {} (map (juxt :value identity)) rows))]
      (is (= (mapv :value lun) (mapv :value vaar))
          "both agents list the same models in the same order")
      (doseq [value (map :value lun)]
        (is (not= (get (by-value lun) value) (get (by-value vaar) value))
            (str "rows for " value " must differ between agents")))))

  (testing "rows differ even when the two agents share a resolution"
    (let [lun (picker/agent-model-rows "room-a" "lun-id" "Lun"
                                       (resolution "gpt-*-sol") choices)
          vaar (picker/agent-model-rows "room-a" "var-id" "Vár"
                                        (resolution "gpt-*-sol") choices)]
      (is (every? true? (map not= lun vaar))))))

(deftest rows-of-one-agent-in-two-rooms-are-never-equal-items
  (let [first-room (picker/agent-model-rows
                    "room-a" "lun-id" "Lun" (resolution "gpt-*-sol") choices)
        second-room (picker/agent-model-rows
                     "room-b" "lun-id" "Lun" (resolution "gpt-*-sol") choices)]
    (is (= (mapv :value first-room) (mapv :value second-room)))
    (is (every? true? (map not= first-room second-room))
        "memoized nodes must not retain another room's completion callback")))

(deftest selection-handler-uses-the-row-s-own-target
  (testing "a stale node's handler still names the room and agent in its row"
    (let [writes (atom [])
          on-select (fn [row]
                      (swap! writes conj [(:room-id row) (:agent-id row)
                                          (:value row)]))
          lun-mini (->> (picker/agent-model-rows
                         "room-a" "lun-id" "Lun"
                         (resolution "gpt-*-sol") choices)
                        (filter #(= "gpt-5.4-mini" (:value %)))
                        first)]
      ((:on-click (picker/option-attrs lun-mini on-select)) {})
      (is (= [["room-a" "lun-id" "gpt-5.4-mini"]] @writes)
          "write and completion refresh targets both come from the row"))))

(deftest inheritance-row-leads-and-tracks-the-override
  (let [rows (picker/agent-model-rows "room-a" "lun-id" "Lun"
                                      (resolution "gpt-*-sol") choices)
        [inherit & catalog] rows]
    (is (= :inheritance (:kind inherit)))
    (is (false? (:selected? inherit)) "an overriding agent does not inherit")
    (is (= ["gpt-*-sol"] (map :value (filter :selected? catalog)))
        "an :auto? resolution ticks the family row, not a version")
    (is (every? #(= "Lun" (:agent-name %)) rows)))

  (testing "with no override the inheritance row is the ticked one"
    (let [rows (picker/agent-model-rows "room-a" "lun-id" "Lun"
                                        (assoc (resolution "gpt-*-sol") :configured? false)
                                        choices)]
      (is (true? (:selected? (first rows))))
      (is (empty? (filter :selected? (rest rows)))))))

(deftest explicit-version-resolution-ticks-that-version
  (let [rows (picker/agent-model-rows
              "room-a" "lun-id" "Lun"
              (assoc (resolution "gpt-*-sol")
                     :auto? false
                     :preferred-model "gpt-5.4-mini")
              choices)]
    (is (= ["gpt-5.4-mini"] (map :value (filter :selected? rows))))))
