(ns is.simm.model.fake-models-server
  "Local model-list fixture. Responses are scoped by path, credential and page
   cursor, so tests can prove that identical URLs still receive two provider
   requests and that a paginated contract is walked to its end.

   The credential is read from EITHER presentation — `Authorization: Bearer`
   or Anthropic's `x-api-key` — and every request's headers and query string
   are recorded, so a test can assert that a provider was addressed under its
   own contract rather than another provider's.

   Fixture credentials are literals invented here. No test reads a real
   provider key, and none is ever logged or asserted on."
  (:require [clojure.string :as str]
            [jsonista.core :as json])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress URLDecoder]
           [java.nio.charset StandardCharsets]))

(defn openai-models-body
  "The OpenAI Models-API envelope: what OpenAI documents, and what Fireworks'
   inference base is observed to answer."
  [models]
  {:object "list"
   :data (mapv (fn [id] {:id id :object "model"}) models)})

(defn anthropic-models-body
  "One page of Anthropic's documented List models envelope.

   `has_more`, `first_id` and `last_id` are members of every response, and
   `last_id` is what the caller passes back as `after_id`. Members beside `id`
   mirror the documented ModelInfo so a test proves they are ignored rather
   than merely absent."
  ([models] (anthropic-models-body models false))
  ([models has-more?]
   {:data (mapv (fn [id]
                  {:id id
                   :type "model"
                   :display_name id
                   :created_at "2026-07-24T00:00:00Z"
                   :max_input_tokens 1000000
                   :max_tokens 128000
                   :capabilities {:batch {:supported true}}})
                models)
    :has_more has-more?
    :first_id (first models)
    :last_id (last models)}))

(defn- response-bytes [body]
  (.getBytes (json/write-value-as-string body) StandardCharsets/UTF_8))

(defn- decode [s] (URLDecoder/decode (str s) StandardCharsets/UTF_8))

(defn- query-params
  "Query string as a map of decoded string keys to decoded string values."
  [query]
  (into {}
        (comp (remove str/blank?)
              (map #(let [[k v] (str/split % #"=" 2)]
                      [(decode k) (decode (or v ""))])))
        (str/split (or query "") #"&")))

(defn- credential-of
  "The bare credential, whichever header presented it."
  [^HttpExchange exchange]
  (let [headers (.getRequestHeaders exchange)
        bearer (.getFirst headers "Authorization")]
    (if (and bearer (str/starts-with? bearer "Bearer "))
      (subs bearer (count "Bearer "))
      (.getFirst headers "x-api-key"))))

(defn- handle! [responses requests ^HttpExchange exchange]
  (let [uri (.getRequestURI exchange)
        path (.getPath uri)
        headers (.getRequestHeaders exchange)
        params (query-params (.getQuery uri))
        credential (credential-of exchange)
        {:keys [status body]
         :or {status 503 body (openai-models-body [])}}
        (get @responses [path credential (get params "after_id")])
        payload (response-bytes body)]
    (swap! requests conj {:path path
                          :authorization (.getFirst headers "Authorization")
                          :api-key (.getFirst headers "x-api-key")
                          :anthropic-version (.getFirst headers "anthropic-version")
                          :query params})
    (.add (.getResponseHeaders exchange) "Content-Type" "application/json")
    (.sendResponseHeaders exchange status (alength payload))
    (with-open [out (.getResponseBody exchange)]
      (.write out payload))))

(defn with-server [f]
  (let [responses (atom {})
        requests (atom [])
        server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext
     server "/"
     (reify HttpHandler
       (handle [_ exchange]
         (handle! responses requests exchange))))
    (.start server)
    (try
      (f {:base-url (str "http://127.0.0.1:" (.getPort (.getAddress server)))
          :responses responses
          :requests requests})
      (finally
        (.stop server 0)))))

(defn respond-at-cursor!
  "Answer `path` for this credential AT ONE page cursor. `after` is nil for the
   first page and the previous page's `last_id` thereafter."
  [{:keys [responses]} path credential after status body]
  (swap! responses assoc [path credential after] {:status status :body body}))

(defn respond-with!
  "Answer `path` with an exact status and body for this credential. Tests that
   need a foreign schema, a truncated page, or a rejection use this directly."
  [fixture path credential status body]
  (respond-at-cursor! fixture path credential nil status body))

(defn respond!
  "A successful model list in the OpenAI-shaped contract this code parses."
  [fixture path credential models]
  (respond-with! fixture path credential 200 (openai-models-body models)))

(defn anthropic-pages!
  "A successful Anthropic list delivered as `pages`, one vector of ids each.

   Every page but the last sets `has_more` and is fetched with the previous
   page's `last_id` as `after_id`, which is exactly how the documented cursor
   walk behaves. A single page is therefore just `[[id ...]]`."
  [fixture path credential pages]
  (loop [[page & more] pages
         after nil]
    (when page
      (respond-at-cursor! fixture path credential after 200
                          (anthropic-models-body page (boolean (seq more))))
      (recur more (last page)))))

(defn outage!
  "A temporary provider failure: the endpoint answers, badly, and recovery is a
   matter of waiting."
  [fixture path credential]
  (respond-with! fixture path credential 503 (openai-models-body [])))

(defn reject-credential!
  "The provider answered and refused the key. The body mirrors what Fireworks
   returns for an invalid key; it carries no credential value."
  [fixture path credential]
  (respond-with! fixture path credential 401
                 {:error {:message "The API key you provided is invalid."
                          :code "UNAUTHORIZED"
                          :type "error"}}))

(defn reject-anthropic-credential!
  "Anthropic answering that this key will not work.

   The body mirrors Anthropic's documented error envelope, which names the
   error TYPE rather than a code. It carries no credential value."
  [fixture path credential status]
  (respond-with! fixture path credential status
                 {:type "error"
                  :error {:type (if (= 403 status)
                                  "permission_error"
                                  "authentication_error")
                          :message "invalid x-api-key"}}))
