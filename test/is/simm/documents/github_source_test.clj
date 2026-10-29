(ns is.simm.documents.github-source-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.github-source :as source]))

(deftest public-identities-have-one-canonical-network-target
  (doseq [input ["Owner/Repo" "owner/repo.git" "Owner/Repo.GIT"
                 "https://github.com/Owner/Repo" "https://github.com/Owner/Repo.git/"]]
    (let [identity (source/repository! input)]
      (is (= "owner/repo" identity))
      (is (= identity (source/repository! identity)))
      (is (= (str "https://github.com/" identity ".git")
             (source/public-url (source/source! {:kind :github :repository input}))))))
  (is (= "https://github.com/owner/repo.git"
         (source/public-url {:kind :github :repository "Owner/Repo"})))
  (is (= "owner/.github" (source/repository! "owner/.github"))))

(deftest source-cannot-select-credentials-hosts-refs-or-path-traversal
  (doseq [input [nil "" " owner/repo" "owner/repo " "owner/repo/"
                 "owner/repo/tree/main" "owner/.." "owner/." "owner/re%70o" "owner/repo.git.git" "Owner/Repo.GIT.git"
                 "owner/repo?ref=main" "owner/repo#main" "owner/repo\n"
                 "https://user@github.com/owner/repo" "https://github.com:443/owner/repo"
                 "https://github.com.evil/owner/repo" "http://github.com/owner/repo"
                 "git@github.com:owner/repo" "file:///owner/repo"
                 "https://github.com/owner/repo//" (str "owner/" (apply str (repeat 101 "x")))]]
    (is (thrown? clojure.lang.ExceptionInfo (source/repository! input)) (pr-str input)))
  (doseq [descriptor [{:kind :github :repository "owner/repo" :token "secret"}
                      {:kind :github :repository "owner/repo" :ref "main"}
                      {:kind :git :repository "owner/repo"} nil]]
    (is (thrown? clojure.lang.ExceptionInfo (source/source! descriptor)))))
