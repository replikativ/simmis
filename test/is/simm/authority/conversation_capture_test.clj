(ns is.simm.authority.conversation-capture-test
  "Real inbound policy, captured application owner, native Room and CAS writer."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.authority.world :as world]
            [is.simm.documents.files :as files]
            [is.simm.ops.conversation-capture :as capture]
            [is.simm.ops.conversation-capture-test :as fixture]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.conversation-capture :as protocol]
            [is.simm.uis.web.desktop.conversation-capture-remote :as remote]))

(defn- with-capture [f]
  (@#'fixture/with-fixture
   (fn [{:keys [host capture-owner] :as data}]
     (with-redefs-fn {#'application/installation (atom {:host host :owner capture-owner})}
       #(f (assoc data :inbound
                  (fn [kind op request]
                    (world/inbound! (world/principal (:w data) kind) op
                                    {:room-id-str (str (get-in data [:w :room])) :request request}))))))))

(deftest actual-inbound-captures-human-identity-and-requires-write
  (with-capture
    (fn [{:keys [w room owned attached inbound]}]
      (let [id (@#'fixture/message! room (:owner w) "Explicit room evidence <script>never()</script>" {:role :user})
            before @owned other @attached]
        (doseq [kind [:non-member :admin :anon]
                op ["preview-conversation-capture!" "publish-conversation-capture!"]]
          (is (world/refused? (inbound kind op {:message-ids [id]}))))
        (let [before-input (inputs/selection @owned inputs/default-policy)
              p (:result (inbound :member "preview-conversation-capture!" {:message-ids [id]}))]
          (is (protocol/preview? p))
          (is (= before @owned) "Preview makes no files or commit")
          (let [request {:message-ids [id] :capture-id (:capture-id p) :expected-target-stamp (:target-stamp p)}
                saved (:result (inbound :member "publish-conversation-capture!" request))]
            (is (protocol/saved? saved))
            (is (= (:text p) (:text (files/read-file-in @owned (:path p)))))
            (is (= (:capture-id p) (:capture-id saved)))
            (let [frozen (inputs/capture @owned inputs/default-policy)
                  unchanged (inputs/selection @owned inputs/default-policy)]
              (is (= (:text p) (get-in frozen [:inputs (str "/in/knowledge/sources/" (:path p))])))
              (is (not= (:trigger-id before-input) (:trigger-id frozen)))
              (is (= (:trigger-id frozen) (:trigger-id unchanged)))
              (is (= "# Owned\n" (:text (files/read-file-in @owned "wiki/file.md")))))
            (is (= {:status :inspection-required}
                   (:result (inbound :member "publish-conversation-capture!" request))))))
        (is (= other @attached))))))

(deftest native-error-reasons-never-invent-replay-authority-or-leak-data
  (with-capture
    (fn [{:keys [inbound w]}]
      (let [request {:message-ids [(random-uuid)]}
            error (fn [& _] (throw (ex-info "private body" {:reason :safe-to-retry :secret "private"})))]
        (with-redefs [capture/preview! error capture/publish! error]
          (is (= {:status :unavailable}
                 (:result (inbound :owner "preview-conversation-capture!" request))))
          (is (= {:status :inspection-required}
                 (:result (inbound :owner "publish-conversation-capture!" request)))))))))

(deftest queued-browser-body-retains-the-original-application-owner
  (with-capture
    (fn [{:keys [w capture-owner host]}]
      (let [body (remote/prepare-operation (:owner w) (str (:room w)) :publish {})
            replacement (lifetime/create)
            calls (atom 0)]
        (lifetime/open! replacement)
        (lifetime/fence! capture-owner)
        (with-redefs-fn {#'application/installation (atom {:host host :owner replacement})
                        #'capture/publish! (fn [& _] (swap! calls inc))}
          #(do (is (= {:status :inspection-required} (body))) (is (zero? @calls))))))))

(deftest room-evidence-inbound-uses-current-room-writer-and-retains-each-original
  (with-capture
    (fn [{:keys [w room owned attached inbound]}]
      (let [id (@#'fixture/message! room (:owner w) "Original room evidence" {:role :user})
            other @attached]
        (doseq [kind [:non-member :admin :anon]
                op ["preview-room-evidence!" "publish-room-evidence!"]]
          (is (world/refused? (inbound kind op {}))))
        (let [p (:result (inbound :member "preview-room-evidence!" {}))
              request {:capture-id (:capture-id p) :expected-target-stamp (:target-stamp p)}
              saved (:result (inbound :member "publish-room-evidence!" request))]
          (is (protocol/room-preview? p))
          (is (= [id] (mapv :message-id (:artifacts p))))
          (is (protocol/room-saved? saved p))
          (doseq [a (:artifacts p)]
            (is (= (:text a) (:text (files/read-file-in @owned (:path a))))))
          (is (= {:status :inspection-required}
                 (:result (inbound :member "publish-room-evidence!" request)))))
        (is (= other @attached))))))
