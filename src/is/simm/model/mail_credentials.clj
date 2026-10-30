(ns is.simm.model.mail-credentials
  "Server-only encryption for mail connection secrets.

   The browser only receives `:credentials-configured?`; passwords and the
   encrypted payload never cross a remote boundary. Production should set
   SIMMIS_CREDENTIAL_KEY to a base64url-encoded 32-byte key. Local development
   gets a persistent key in <home>/mail-credential-key (`.dvergr` by default)."
  (:require [clojure.edn :as edn]
            [is.simm.runtimes.instance :as instance]
            [clojure.string :as str]
            [org.replikativ.geheimnis.aead :as aead]
            [org.replikativ.geheimnis.codec :as codec]
            [org.replikativ.geheimnis.core :as geheimnis]
            [taoensso.telemere :as log]))

(def ^:private key-size 32)
(def ^:private nonce-size 12)

(defn- key-file [] (java.io.File. ^String (instance/home-path "mail-credential-key")))

(defn- persistent-key [^java.io.File f]
  (let [path (.toPath (.getAbsoluteFile f))
        permissions (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------")]
    (java.nio.file.Files/createDirectories (.getParent path) (make-array java.nio.file.attribute.FileAttribute 0))
    (try
      ;; CREATE_NEW chooses one owner and never overwrites another process's
      ;; key. An incomplete failed write remains an invalid file, not permission
      ;; to silently rotate a key protecting existing encrypted credentials.
      (java.nio.file.Files/createFile path (into-array java.nio.file.attribute.FileAttribute
                                                       [(java.nio.file.attribute.PosixFilePermissions/asFileAttribute permissions)]))
      (spit f (codec/bytes->b64url (geheimnis/random-bytes key-size)))
      (log/log! {:level :warn :id ::dev-credential-key-created
                 :msg "Generated a local credential key; set SIMMIS_CREDENTIAL_KEY in production"})
      (catch java.nio.file.FileAlreadyExistsException _ nil))
    (when (or (java.nio.file.Files/isSymbolicLink path)
              (not (java.nio.file.Files/isRegularFile path (into-array java.nio.file.LinkOption [java.nio.file.LinkOption/NOFOLLOW_LINKS]))))
      (throw (ex-info "Credential key must be a private regular file" {})))
    (java.nio.file.Files/setPosixFilePermissions path permissions)
    (str/trim (slurp f))))

(defn- load-key []
  (let [encoded (or (some-> (System/getenv "SIMMIS_CREDENTIAL_KEY") str/trim not-empty)
                    (persistent-key (key-file)))
        key (codec/b64url->bytes encoded)]
    (when-not (= key-size (alength key))
      (throw (ex-info "SIMMIS_CREDENTIAL_KEY must decode to exactly 32 bytes"
                      {:decoded-bytes (alength key)})))
    key))

(defonce ^:private master-key (delay (load-key)))

(defn encrypt
  "Encrypt an EDN-safe value for `account-id`. Returns registry-safe strings."
  [account-id value]
  (let [nonce (geheimnis/random-bytes nonce-size)
        aad (codec/str->bytes (str "simmis/mail-account/" account-id))
        plaintext (codec/str->bytes (pr-str value))
        ciphertext (aead/aead-encrypt-sync @master-key nonce aad plaintext)]
    {:nonce (codec/bytes->b64url nonce)
     :ciphertext (codec/bytes->b64url ciphertext)}))

(defn decrypt
  "Decrypt a value previously returned by `encrypt`."
  [account-id nonce ciphertext]
  (let [aad (codec/str->bytes (str "simmis/mail-account/" account-id))]
    (->> (aead/aead-decrypt-sync @master-key
                                 (codec/b64url->bytes nonce)
                                 aad
                                 (codec/b64url->bytes ciphertext))
         codec/bytes->str
         edn/read-string)))
