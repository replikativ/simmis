(ns is.simm.starter.wiki-maintenance-v6-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.knowledge-scope :as scope]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-coverage :as coverage]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.starter.install-wiki :as install]
            [is.simm.starter.wiki-maintenance :as starter]))

(def original-company
  "---\nsimmis-id: 8f36f3f0-72b4-447e-baaa-36da121045e7\nrevenue: 0\nactive: false\ncustom:\n  owner-note: keep me\n---\n# Harbor Labs\n\nHarbor Labs is exploring invoice tools.\n")

(def sam-source
  "Synthetic single saved human message. Sam says: I am exploring an invoice tool for independent studios. I have not validated demand or interviewed customers. This message was omitted from the navigation digest.\n")

(def capture-source
  "Synthetic brief capture description. A sampled screenshot description mentions Cedar Works in a browser tab. No client relationship is established. The screenshot bytes were not inspected by the maintainer; recording position is estimated.\n")

(def sam-input "/in/knowledge/sources/inbox/conversations/sam.md")
(def capture-input "/in/knowledge/sources/captures/company.md")

(def sam-page
  (str "---\nsimmis-id: 1b851181-7660-4dd0-9894-100427861cd1\n---\n# Sam\n\n"
       "Sam is exploring an invoice tool for independent studios. Demand is unvalidated; no customer interviews are reported.\n\n"
       "[Saved message](" sam-input ")\n\n[Company overview](Company.md)\n"))

(def company-page
  (str original-company "\n## People and evidence\n\n[Sam](Sam.md) is exploring an invoice tool for independent studios.\n\n"
       "A sampled screenshot description mentions Cedar Works in a browser tab; no client relationship is established. "
       "The maintainer did not inspect screenshot bytes. Recording position is estimated.\n\n"
       "[Saved message](" sam-input ") [Capture description](" capture-input ")\n"))

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Version 6 original evidence fixtures"}) (f conn)
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- native-case [sources context statuses reference expected]
  (with-repository
    (fn [conn]
      (doseq [[path text] (merge sources context)]
        (repo/write! conn path (markdown/source-bytes text)))
      (let [policy (inputs/declaration (starter/source 6))
            selected (inputs/selection @conn policy)
            captured (inputs/capture-files @conn policy
                                            (mapv #(dissoc % :input)
                                                  (filter #(= :trigger (:role %)) (get-in selected [:manifest :files]))))
            triggers (filter #(= :trigger (:role %)) (get-in captured [:manifest :files]))
            items (mapv (fn [{:keys [input content]}]
                          (merge {:input input :content content} (get statuses input))) triggers)
            report {:version 1 :trigger-id (:trigger-id captured) :items items}
            output (assoc reference coverage/output-path (pr-str report))
            source-texts (into {} (map (fn [[path text]] [(str "/in/knowledge/sources/" path) text])) sources)
            fixtures (merge (:inputs captured) (into {} (map (fn [[path text]] [(str "/" path) text])) context))
            gold {:version 1 :expected expected :sources source-texts
                  :coverage-required (vec (concat [(str (:trigger-id captured))]
                                                  (mapcat (fn [{:keys [input content status]}]
                                                            [input (str content) (str status)]) items)))}]
        {:fixtures fixtures :gold gold :reference output :report report :captured captured}))))

(defn evaluation-cases
  "Build exact frozen manifests through the native repository collector. No provider."
  []
  {"original-coverage"
   (native-case
    {"inbox/conversations/sam.md" sam-source "captures/company.md" capture-source}
    {"wiki/Company.md" original-company}
    {sam-input {:status :incorporated :targets ["wiki/Company.md" "wiki/Sam.md"]
                :note "The original omitted from the digest supports Sam's note and the overview link."}
     capture-input {:status :incorporated :targets ["wiki/Company.md"]
                    :note "Attribute the sampled description without inferring a client relationship."}}
    {"/out/knowledge/Sam.md" sam-page "/out/knowledge/Company.md" company-page}
    {"/out/knowledge/Sam.md" {:required ["# Sam" "independent studios" "unvalidated"]
                              :preserved ["simmis-id: 1b851181-7660-4dd0-9894-100427861cd1"]
                              :forbidden ["validated demand" "paying customer"] :citations [sam-input]}
     "/out/knowledge/Company.md" {:required ["[Sam](Sam.md)" "Cedar Works" "no client relationship"
                                             "did not inspect screenshot bytes" "estimated"]
                                  :preserved [original-company] :forbidden ["Cedar Works is a client"]
                                  :citations [sam-input capture-input]}})
   "no-change"
   (native-case
    {"inbox/conversations/acknowledgement.md" "Synthetic original message: Thanks, no additional project information.\n"}
    {"wiki/Company.md" original-company}
    {"/in/knowledge/sources/inbox/conversations/acknowledgement.md"
     {:status :no-change :targets [] :note "The acknowledgement adds no substantive knowledge claim."}}
    {} {})})

(defn evaluation-source [cases]
  (merge (starter/source 6)
         {"benchmark.edn" (pr-str {:version 1 :splits {:development ["original-coverage"] :held-out ["no-change"]}})}
         (into {} (mapcat (fn [[id {:keys [fixtures gold]}]]
                           [[(str "cases/" id "/fixture-pack.edn")
                             (pr-str {:kind :dvergr/fixture-pack :version 1 :files fixtures})]
                            [(str "cases/" id "/gold.edn") (pr-str gold)]])) cases)))

(defn write-evaluation!
  "Explicit build helper: materialize this separate native-generated fixture bundle."
  [directory]
  (let [cases (evaluation-cases) files (evaluation-source cases)]
    (doseq [[path text] files]
      (let [file (io/file directory path)]
        (io/make-parents file)
        (spit file text)))
    {:files (count files) :cases (vec (sort (keys cases)))}))

(deftest current-starter-is-v6-and-historical-source-stays-explicit
  (let [source (starter/source) policy (inputs/declaration source)
        snapshot (fn [files] {:files files :manifest (mapv (fn [path] {:path path :mode 33188}) (keys files))})]
    (is (= 6 starter/version))
    (is (= source (starter/source 6)))
    (is (= coverage/declaration-value (coverage/declaration source)))
    (is (= 2 (:version policy)))
    (is (= "wiki" (get-in policy [:output-scope :destination-root])))
    (is (= "out/knowledge" (get-in policy [:output-scope :output-root])))
    (is (= ["captures" "inbox" "transcripts"] (:trigger-roots policy)))
    (doseq [version (range 1 6)]
      (is (not (contains? (starter/source version) "coverage-policy.edn"))))
    (is (= :upgrade-available (:status (install/classify (snapshot (starter/source 5))))))
    (is (= :customized (:status (install/classify (snapshot (assoc (starter/source 5) "local.txt" "Keep my edit"))))))
    (is (= :installed (:status (install/classify (snapshot source)))))
    (is (nil? (:gold (catalog/bundle starter/name source))))
    (is (empty? (:cases (catalog/bundle starter/name source))))))

(deftest native-generated-v2-cases-check-originals-overview-and-coverage
  (let [cases (evaluation-cases) source (evaluation-source cases)
        live (catalog/bundle starter/name (starter/source 6))
        evaluation (catalog/bundle starter/name source)
        checker (get source "checker.clj")
        verdict (fn [files gold] (checkers/run-checker checker {:files files :gold gold :params {} :fetched {} :judgements {}}))]
    (is (not= (:id live) (:id evaluation)))
    (is (= (:definition live) (:definition evaluation)))
    (doseq [[_ {:keys [captured fixtures reference gold report]}] cases]
      (is (= 2 (get-in captured [:manifest :version])))
      (is (= captured (inputs/verify-capture (inputs/declaration source) captured)))
      (is (= 1.0 (:reward (verdict reference gold))))
      (is (= :ready (:status (coverage/inspect
                             (coverage/expected (:manifest captured)) report
                             (set (mapcat :targets (filter #(= :incorporated (:status %)) (:items report))))
                             #(try (scope/target-path! (inputs/output-scope (inputs/declaration source)) %) true
                                   (catch Exception _ false))))))
      (let [fixture-source (assoc (starter/source 6) "fixture-pack.edn"
                                  (pr-str {:kind :dvergr/fixture-pack :version 1 :files fixtures}))
            bundle (catalog/bundle starter/name fixture-source)
            prepared (runs/prepare-source {:files fixture-source}
                                          {:name starter/name :expected-bundle (:id bundle)})]
        (is (= fixtures (select-keys (:inputs prepared) (keys fixtures))))))
    (let [{:keys [reference gold]} (get cases "original-coverage")]
      (is (false? (get-in (verdict (dissoc reference "/out/knowledge/Sam.md") gold) [:checks :expected-pages?])))
      (is (false? (get-in (verdict (dissoc reference coverage/output-path) gold) [:checks :coverage-report?])))
      (is (false? (get-in (verdict (update reference "/out/knowledge/Company.md" str "\nCedar Works is a client") gold)
                          [:checks :forbidden-content?])))
      (is (false? (get-in (verdict (assoc reference "/out/knowledge/Company.md" "# Replaced metadata") gold)
                          [:checks :preserved-content?]))))))
