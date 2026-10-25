(ns is.simm.documents.media-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.documents.links :as links]
            [is.simm.documents.media :as media]))

(deftest only-inert-known-formats-have-inline-media-kinds
  (is (= :image (media/kind "images/Future.JPG")))
  (is (= :video (media/kind "capture/recording.mp4")))
  (is (= "audio/ogg" (media/content-type "meeting.ogg")))
  (is (nil? (media/kind "diagram.svg")))
  (is (nil? (media/kind "page.html")))
  (is (nil? (media/kind "file.mp4?other"))))

(deftest portable-evidence-retains-content-identity-and-exact-path
  (let [repository #uuid "0490e6e3-a857-48ae-a47f-d78fd328628e"
        content #uuid "0b0b79e4-9ad3-4f7d-906b-998f86b80777"
        reference (media/reference repository "captures/screen.jpg" {:content content :size 123 :mode 33188})]
    (is (= {:repository repository :path "captures/screen.jpg" :content content :size 123} reference))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (media/reference repository "../private.jpg" {:content content :size 123})))
    (is (= (str "/blobs/workspaces/" repository "?path=captures%2Fscreen%20one.jpg&content=" content "&target=" repository)
           (links/media-route {:room repository :path "captures/screen one.jpg" :content content :target repository})))
    (is (nil? (links/media-route {:room repository :path "/private.jpg" :content content :target repository})))))

(deftest memory-tab-room-identities-use-canonical-wire-strings
  (let [id #uuid "0490e6e3-a857-48ae-a47f-d78fd328628e" fields {:path "frame.png" :content id :target id}]
    (is (= (links/media-route (assoc fields :room id))
           (links/media-route (assoc fields :room (str id)))))
    (is (nil? (links/media-route (assoc fields :room "1-2-3-4-5"))))))
