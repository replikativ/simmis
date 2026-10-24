(ns is.simm.uis.web.desktop.memory-backlinks
  "Pure ownership of a saved-workspace backlink observation. No IO or handles.")

(defn marker [room state connection roster]
  (let [path (:selected state) draft (get-in state [:drafts path])
        source (:source draft)]
    (when (and connection (:target-verified? state) (= connection (:verified-connection state))
               (= connection (:connection state)) (uuid? (:target-stamp state))
               (uuid? (:definition-basis state)) (uuid? (:repository-id source))
               (= :markdown (:kind source)) (string? path) (:entry source)
               (not (:target-stale? draft)) (= (:target-stamp state) (:target-stamp source))
               (some #(= room (str (:room/id %))) (:rooms roster)))
      {:path path :connection connection :roster roster
       :target-stamp (:target-stamp state) :target-epoch (:target-epoch state)
       :read-epoch (:read-epoch state) :definition-basis (:definition-basis state)
       :repository-id (:repository-id source) :entry (:entry source)})))

(defn request [marker]
  {:expected-target-stamp (:target-stamp marker) :repository-id (:repository-id marker)
   :target {:kind :file :path (:path marker)}})

(defn reply [state marker]
  (let [cached (:backlinks state) result (:reply cached)]
    (when (and marker (= marker (:marker cached))
               (= (:target-stamp marker) (:target-stamp result))
               (= (:repository-id marker) (:repository-id result))
               (= (:path marker) (get-in result [:target :path]))
               (uuid? (:source-basis result))
               (#{:ready :missing :limited} (:status result)) (vector? (:results result)))
      result)))

(defn observation [state marker]
  (let [cached (:backlinks state) current? (and marker (= marker (:marker cached)))]
    {:open? (boolean (:open? cached)) :reply (reply state marker)
     :error (when current? (:error cached))
     :loading? (and current? (true? (get-in state [:loading :backlinks])))}))
