(ns is.simm.runtimes.alpha-memory-test
  (:require [clojure.test :refer [deftest is]]
            [kabel.auth.store.protocol :as store]
            [is.simm.model.knowledge-bases :as kb]
            [is.simm.model.personal-rooms :as personal]
            [is.simm.runtimes.auth-config :as auth]))

(deftest startup-preserves-existing-memory-without-creating-or-attaching-a-legacy-wiki
  (let [owner (random-uuid) calls (atom [])]
    (with-redefs [auth/alpha-parties (constantly [{:email "preview@example.invalid" :name "Preview"}])
                  auth/get-auth-store (constantly :store)
                  store/find-user-by-email (fn [& _] {:party/id owner})
                  personal/ensure! (fn [id name] (swap! calls conj [id name]) {:room/id (random-uuid)})
                  kb/ensure-default-kb! (fn [& _] (throw (AssertionError. "No extra legacy store")))
                  kb/attach-kb-to-room! (fn [& _] (throw (AssertionError. "Do not rewrite existing grants")))]
      (auth/seed-alpha-parties!)
      (auth/seed-alpha-parties!)
      (is (= [[owner "Preview"] [owner "Preview"]] @calls)))))
