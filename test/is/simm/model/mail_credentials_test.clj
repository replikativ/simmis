(ns is.simm.model.mail-credentials-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [is.simm.model.mail-credentials :as credentials]
            [org.replikativ.geheimnis.codec :as codec])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(deftest persisted-credential-key-is-owner-only-stable-and-never-overwritten
  (let [root (Files/createTempDirectory "simmis-credential-key-" (make-array FileAttribute 0))
        file (io/file (.toFile root) "key") path (.toPath file)
        link (.resolve root "link")]
    (try
      (let [key (#'credentials/persistent-key file)]
        (is (= 32 (alength (codec/b64url->bytes key))))
        (is (= (PosixFilePermissions/fromString "rw-------") (Files/getPosixFilePermissions path (make-array java.nio.file.LinkOption 0))))
        (is (= key (#'credentials/persistent-key file)))
        (Files/setPosixFilePermissions path (PosixFilePermissions/fromString "rw-rw-rw-"))
        (is (= key (#'credentials/persistent-key file)))
        (is (= (PosixFilePermissions/fromString "rw-------") (Files/getPosixFilePermissions path (make-array java.nio.file.LinkOption 0))))
        (Files/createSymbolicLink link path (make-array FileAttribute 0))
        (is (thrown? clojure.lang.ExceptionInfo (#'credentials/persistent-key (.toFile link))))
        (is (= key (slurp file)))
        (spit file "")
        (is (= "" (#'credentials/persistent-key file)) "A failed creation must not silently rotate an existing key"))
      (finally (Files/deleteIfExists link) (Files/deleteIfExists path) (Files/deleteIfExists root)))))

(deftest encrypted-mail-config-roundtrip
  (let [account-id (random-uuid)
        config {:email "person@example.com"
                :imap {:host "imap.example.com" :port 993
                       :user "person" :pass "not-returned-to-browser"}}
        master-key-var (ns-resolve 'is.simm.model.mail-credentials 'master-key)]
    (with-redefs-fn
      {master-key-var (delay (byte-array (map byte (range 32))))}
      (fn []
        (let [{:keys [nonce ciphertext]} (credentials/encrypt account-id config)]
          (testing "the encrypted registry value contains no plaintext secret"
            (is (not (.contains ciphertext "not-returned-to-browser"))))
          (testing "the account UUID is authenticated as AAD"
            (is (= config (credentials/decrypt account-id nonce ciphertext)))
            (is (thrown? Exception
                         (credentials/decrypt (random-uuid) nonce ciphertext)))))))))
