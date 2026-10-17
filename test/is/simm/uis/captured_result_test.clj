(ns is.simm.uis.captured-result-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.chat.schema :as schema]
            [is.simm.uis.web.desktop.captured-result :as result]
            [is.simm.uis.web.desktop.run-detail :as run-detail]))

(deftest captured-attempt-is-associated-by-ref-not-coincidental-id
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg) run (random-uuid) attempt (random-uuid) other (random-uuid)]
      (try
        (schema/ensure-full-schema! conn)
        (d/transact conn [{:run/id run} {:run/id other}
                          {:attempt/id attempt :attempt/run [:run/id run]}])
        (is (= (str attempt) (:attempt-id (run-detail/query-run-detail @conn run))))
        (is (nil? (:attempt-id (run-detail/query-run-detail @conn other))))
        (finally (d/release conn))))))

(def entry {:path "/out/file.txt" :size 4 :content-id "immutable-file"})
(def chunk {:attempt {:id "certified"} :path "/out/file.txt" :size 4 :content-id "immutable-file"
            :encoding "base64" :offset 0 :length 2 :next-offset 2})

(deftest only-exact-contiguous-bounded-chunks-are-accepted
  (is (result/chunk-valid? "certified" entry 0 chunk 2))
  (is (result/chunk-valid? "certified" entry 2 (-> chunk (assoc :offset 2) (dissoc :next-offset)) 2))
  (doseq [bad [(assoc chunk :offset 1) (assoc chunk :path "/another") (assoc chunk :content-id "changed")
               (assoc chunk :size 5) (assoc chunk :length 16385) (assoc chunk :length -1)
               (assoc chunk :length 0 :next-offset 0) (assoc chunk :next-offset 3)
               (assoc chunk :attempt {:id "another"}) (dissoc chunk :next-offset)
               (assoc chunk :length nil) (assoc chunk :encoding "text")]]
    (is (not (result/chunk-valid? "certified" entry 0 bad 2))))
  (is (not (result/chunk-valid? "certified" entry 0 chunk 1)))
  (is (not (result/chunk-valid? "certified" entry nil chunk 2)))
  (is (not (result/chunk-valid? "certified" (assoc entry :size (inc result/max-download-bytes)) 0 chunk 2))))

(deftest empty-files-have-a-valid-terminal-chunk
  (is (result/chunk-valid? "certified" (assoc entry :size 0) 0
                           (-> chunk (assoc :size 0 :length 0) (dissoc :next-offset)) 0)))

(deftest download-name-is-a-safe-leaf-not-a-host-path
  (is (= "result.md" (result/download-name "/out/result.md")))
  (is (= "x_y" (result/download-name "C:\\out\\x:y")))
  (is (= "x_y" (result/download-name "/out/x\ny")))
  (is (= "recorded-output" (result/download-name ".."))))
