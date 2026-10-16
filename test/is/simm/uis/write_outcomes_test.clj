(ns is.simm.uis.write-outcomes-test
  "A browser write that the server did not perform must not look performed.

   Two ways it did: a handler that ANSWERED a refusal as a value — `nil`, or
   `{:status :error …}` — which a client reads as success because its failure
   continuation never runs; and call sites that only logged a failure to the
   console. The call sites now route through `remote.cljc`, whose
   `result-failure` is tested here; the drive handlers now refuse instead of
   answering nil."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.authority.world :as world]
            [is.simm.uis.web.desktop.chat-remote]
            [is.simm.uis.web.desktop.remote :as rem]))

(deftest a-refusal-answered-as-a-value-is-a-failure
  (doseq [r [{:status :error :error :not-a-room-member}
             {:status :error :message "not authenticated"}
             {:status :error :reason :room-not-found}
             {:success false :error :reflected-property :message "\"S/Page/created-at\" reflects data owned by another system"}
             {:status :recovery-required :error "transfer failed"}]]
    (testing (pr-str r)
      (let [e (rem/result-failure r)]
        (is (some? e))
        (is (seq (rem/error-text e)) "and says why"))))
  (doseq [r [{:status :ok} {:success true} {:max-tx 3} nil true 42 #uuid "00000000-0000-0000-0000-000000000001"
             {:status :not-active :run-id "x"} {:status :not-found :handle "h"}]]
    (testing (str "not a failure: " (pr-str r))
      (is (nil? (rem/result-failure r))))))

(deftest a-drive-write-in-a-room-without-a-drive-is-refused-not-nil
  ;; The world's team room has no drive attached. These answered nil, which
  ;; the upload, the new-folder and the delete took for success.
  (world/with-world [_ w]
    (let [member (world/principal w :member)
          room (str (:room w))]
      (doseq [[fn-name args] [["put-drive-file!" {:room-id-str room :dir-path "" :file-name "a.txt"
                                                  :blob-id "b" :mime "text/plain" :size 1}]
                              ["mkdir-drive-path!" {:room-id-str room :dir-path "/x"}]
                              ["delete-drive-node!" {:room-id-str room
                                                     :node-id-str (str (random-uuid))}]]]
        (testing fn-name
          (let [r (world/inbound! member fn-name args)]
            (is (re-find #"no drive" (str (:error r))) (pr-str r))
            (is (not (world/refused? r)) "a member may write the room's drive — there is none")))))))
