(ns is.simm.ops.github-import
  "Fixed HOST initializer for a private, unpublished public GitHub Memory.
   No endpoint, source-selected hooks, subprocesses or imported evaluation."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.git.budget :as budget]
            [geschichte.git.transport :as transport]
            [geschichte.repo :as repo]
            [is.simm.documents.github-source :as github]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.model.workspace-intent :as intent]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [kontor.governance :as governance])
  (:import [java.util.concurrent Semaphore]))

;; Native HOST admission only; never forked UI data or a source cache.
(defonce ^:private admission (Semaphore. 1 true))

(def limits
  {:response-bytes (* 32 1024 1024) :pack-bytes (* 16 1024 1024)
   :objects 50000 :index-bytes (* 4 1024 1024)
   :object-bytes (* 4 1024 1024) :inflated-bytes (* 128 1024 1024)
   :delta-result-bytes (* 4 1024 1024) :reconstructed-bytes (* 128 1024 1024)
   :delta-depth 32 :commit-visits 4096 :tree-entries 5000 :tree-depth 32
   :checkout-files 2000 :checkout-bytes (* 32 1024 1024)})

(defn- refuse! [reason]
  (throw (ex-info "GitHub import could not complete" {:type ::held :reason reason})))

(defn with-admission!
  "Admit one bounded Git operation without queuing a second native owner."
  [f]
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "GitHub import interrupted before admission")))
  (when-not (.tryAcquire admission) (refuse! :busy))
  (try (f) (finally (.release admission))))

(defn reference? [ref]
  (and (string? ref) (<= 12 (count ref) 512)
                 (str/starts-with? ref "refs/heads/")
                 (not (re-find #"[\\\p{Cntrl}]" ref))))

(defn- reference! [ref]
  (when-not (reference? ref)
    (refuse! :unsupported-ref))
  ref)

(defn source-metadata!
  "Read fixed initial provenance from the exclusively prepared native repo.
   This is not source-selected configuration or an authorization lease."
  [conn request]
  (let [source (github/source! (:source request))
        url (github/public-url source)
        ref (reference! (repo/current-ref conn))
        head (:geschichte.commit/id (repo/head-commit conn))
        config (repo/configuration conn)]
    (when-not (and (uuid? head) (:clean? (repo/status conn))
                   (string? (:geschichte.commit/git-oid (repo/commit-by-id conn head)))
                   (= url (get config "remote.origin.url"))
                   (= "origin" (get config (str "branch." (subs ref 11) ".remote")))
                   (= ref (get config (str "branch." (subs ref 11) ".merge"))))
      (refuse! :import-correspondence))
    {:repository (:repository source) :ref ref :head head}))

(defn policies
  "Trusted synchronous create-only policy. Caller holds its original App;
   current! is the fixed same-thread liveness/authority check. Nothing installs
   these operation-local hooks globally or persists executable policy."
  [control-conn creator request current!]
  (lifetime/check-current!)
  (let [source (github/source! (:source request))
        owner (:owner lifetime/*ticket*)
        url (github/public-url source)]
    {space/profile
     {:initialize!
      (fn [conn options]
        (current!)
        ;; Durable immutable source ownership comes BEFORE any network request.
        (d/transact control-conn [[:db.fn/call intent/record-source-tx creator request]])
        (current!)
        (profile/initialize-foundation! conn options)
        (let [worker (Thread/currentThread)
              operation (budget/create
                         {:limits limits :timeout-ms 120000
                          ;; The native HTTP monitor may poll on another thread.
                          ;; Never run same-thread ticket/authority checks there.
                          :cancelled? #(or (.isInterrupted worker) (not (lifetime/ready? owner)))})
              result (transport/clone! {:conn conn :remote "origin" :url url
                                        :options {:import-budget operation
                                                  :http-body-idle-timeout-ms 15000
                                                  :prefixes ["HEAD" "refs/heads/"]}})
              head (repo/head-commit conn)
              selected (repo/commit-by-id conn (:geschichte.commit/id head))
              ref (reference! (repo/current-ref conn))
              config (repo/configuration conn)]
          (budget/check! operation)
          (current!)
          (when-not (and (uuid? (:geschichte.commit/id head))
                         (= (:commit result) (:geschichte.commit/id head))
                         (string? (:git-oid result))
                         (= (:git-oid result) (:geschichte.commit/git-oid selected))
                         (= url (get config "remote.origin.url"))
                         (= "origin" (get config (str "branch." (subs ref 11) ".remote")))
                         (= ref (get config (str "branch." (subs ref 11) ".merge")))
                         (:clean? (repo/status conn)))
            (refuse! :import-correspondence))
          ;; Checkout validated every blob using the SAME operation budget;
          ;; this metadata validation preserves explicit directory constraints.
          (repo/validate-worktree! @conn (repo/worktree conn))
          (profile/prepare-open! conn options)
          (budget/check! operation)
          (current!)
          conn))
      :prepare-open! profile/prepare-open!
      :abort-create! (fn [conn _] (governance/ungovern! conn))}}))
