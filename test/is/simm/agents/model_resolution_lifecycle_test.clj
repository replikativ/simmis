(ns is.simm.agents.model-resolution-lifecycle-test
  (:require [clojure.test :refer [deftest is testing]]
            [dvergr.agent.prompt :as agent-prompt]
            [dvergr.agent.room-context :as room-ctx]
            [dvergr.chat.context :as chat-ctx]
            [dvergr.discourse :as discourse]
            [dvergr.discourse.llm :as llm]
            [dvergr.room.registry :as room-registry]
            [dvergr.tools :as tools]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.drives :as drives]
            [is.simm.model.rooms :as rooms]))

(defn- room-agents-var [sym]
  (ns-resolve 'is.simm.agents.room-agents sym))

(deftest source-room-joins-with-one-knowledge-vocabulary-and-keeps-memory-links
  (let [room-id (random-uuid) agent-id (random-uuid) installed (atom #{})
        offered (atom nil) participant (atom nil)
        installers '[add-source-memory-ns! add-wiki-ns! add-kb-write-ns!
                     add-proposal-ns! add-book-ns! add-sheet-ns! add-office-ns!
                     add-screen-ns! add-transcript-ns! add-workflow-ns! add-web-ns!]
        redefs {(room-agents-var 'room-repo-scope) (constantly (random-uuid))
                (room-agents-var 'get-room-kb-conns)
                (fn [& _] (throw (ex-info "Source room must not load legacy KBs" {})))
                (room-agents-var 'persona+tools) (constantly "A test agent")
                #'rooms/get-room-budget-dollars (constantly 1.0)
                #'rooms/get-room (constantly {:room/slug "source-vocabulary"})
                #'drives/get-room-drives (constantly [])
                #'room-ctx/ensure-ctx! (fn [& _] {})
                #'chat-ctx/sci-context-in (fn [& _] :sandbox)
                #'agent-prompt/assemble-system-prompt
                (fn [prompt opts] (reset! offered (:tools opts)) prompt)
                #'tools/registry (atom (into {} (for [name ["clojure_eval" "knowledge_add" "knowledge_search"]]
                                                 [name {:name name :execute (fn [& _] nil)}])))
                #'llm/llm-agent (fn [opts] (reset! participant opts) opts)
                #'discourse/join (fn [& _] nil)}
        redefs (into redefs (for [name installers]
                             [(room-agents-var name) (fn [& _] (swap! installed conj name))]))]
    (try
      (with-redefs-fn redefs
        (fn []
          (@(room-agents-var 'join-participant!) {:ctx nil} room-id
           {:party/id agent-id :party/display-name "Source probe"} nil
           {:provider :openai :model "test"})
          (is (contains? @installed 'add-source-memory-ns!))
          (is (not-any? @installed '[add-wiki-ns! add-kb-write-ns!]))
          (is (contains? @offered "clojure_eval"))
          (is (not-any? @offered ["knowledge_add" "knowledge_search"]))
          (is (= #{"clojure_eval"} (set (keys (:tools @participant)))))
          (is (.contains ^String (get-in @participant [:spec :system-prompt]) "memory/write!"))
          (is (= {:content "See [[A]]" :notes []}
                 ((:on-reply @participant) "See [[A]]")))))
      (finally (swap! @(room-agents-var 'joined) disj [room-id agent-id])))))

(deftest joined-participant-keeps-captured-spec-until-explicit-reset
  (let [room-id (random-uuid)
        agent-id (random-uuid)
        actor-id (room-agents/party->actor-kw agent-id)
        room {:ctx nil :participants (atom {})}
        agent {:party/id agent-id :party/display-name "Lifecycle probe"}
        desired (atom {:model "gpt-5.5-luna"
                       :provider :openai
                       :available? true})
        resolution-calls (atom [])
        participant-options (atom [])
        installed (atom [])
        vocab-installers '[add-wiki-ns! add-kb-write-ns! add-proposal-ns!
                           add-book-ns! add-sheet-ns! add-office-ns!
                           add-screen-ns! add-transcript-ns! add-workflow-ns!
                           add-web-ns!]
        redefs {(room-agents-var 'get-room-kb-conns) (constantly [])
                (room-agents-var 'persona+tools) (constantly "test prompt")
                #'room-agents/describe-model-resolution
                (fn [_]
                  (swap! resolution-calls conj @desired)
                  @desired)
                #'rooms/get-room-budget-dollars (constantly 1.0)
                #'rooms/get-room (constantly {:room/slug "model-resolution-test"})
                #'agent-prompt/assemble-system-prompt (fn [prompt _] prompt)
                #'room-ctx/ensure-ctx! (fn [& _] {:sci-component ::sandbox-ref})
                ;; The ctx carries a stable REFERENCE; the interpreter is
                ;; resolved in a world. Reading the interpreter off the ctx
                ;; directly installs nothing.
                #'chat-ctx/sci-context-in (fn [cctx _] (some-> (:sci-component cctx)
                                                               (vector :sandbox)))
                #'room-ctx/drop-ctx! (fn [& _] nil)
                #'drives/get-room-drives (constantly [])
                #'tools/registry (atom {})
                #'llm/llm-agent
                (fn [opts]
                  (swap! participant-options conj opts)
                  {:id (:id opts) :spec (:spec opts)})
                #'discourse/join
                (fn [r participant]
                  (swap! (:participants r) assoc (:id participant) participant)
                  participant)
                #'discourse/leave
                (fn [r participant-id]
                  (swap! (:participants r) dissoc participant-id)
                  nil)
                #'room-registry/lookup (constantly room)}
        redefs (into redefs
                     (for [sym vocab-installers]
                       [(room-agents-var sym)
                        (fn [sci-ctx & _] (swap! installed conj [sym sci-ctx]) nil)]))]
    (with-redefs-fn
      redefs
      (fn []
        (room-agents/ensure-agent-joined! room room-id agent nil)
        (testing "the first join captures the current desired resolution"
          (is (= {:provider :openai :model "gpt-5.5-luna"}
                 (select-keys (:spec (get @(:participants room) actor-id))
                              [:provider :model])))
          (is (string? (get-in @(:participants room)
                               [actor-id :spec :system-prompt])))
          (is (= 1 (count @participant-options)))
          (is (= 1 (count @resolution-calls))))

        (testing "the simmis vocabulary lands in the interpreter resolved for the room"
          (is (= (set vocab-installers) (set (map first @installed))))
          (is (every? #(= [::sandbox-ref :sandbox] (second %)) @installed)))

        (reset! desired {:model "gpt-5.6-luna"
                         :provider :openai
                         :available? true})
        (room-agents/ensure-agent-joined! room room-id agent nil)
        (testing "an already joined participant keeps its captured spec"
          (is (= "gpt-5.5-luna"
                 (get-in @(:participants room) [actor-id :spec :model])))
          (is (= 1 (count @participant-options)))
          (is (= 1 (count @resolution-calls))
              "ordinary redispatch does not resolve the model again"))

        (room-agents/reset-agent-contexts! agent-id)
        (is (nil? (get @(:participants room) actor-id)))

        (room-agents/ensure-agent-joined! room room-id agent nil)
        (testing "an explicit reset makes the next join resolve again"
          (is (= "gpt-5.6-luna"
                 (get-in @(:participants room) [actor-id :spec :model])))
          (is (= ["gpt-5.5-luna" "gpt-5.6-luna"]
                 (mapv #(get-in % [:spec :model]) @participant-options)))
          (is (= 2 (count @resolution-calls))))

        ;; Leave the process-global join cache clean for later tests.
        (room-agents/reset-agent-contexts! agent-id)))))
