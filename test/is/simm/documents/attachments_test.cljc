(ns is.simm.documents.attachments-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.documents.attachments :as attachments]))

(deftest ordinary-relative-images-resolve-within-the-room-only
  (doseq [[url expected] [["images/frame.png" "captures/images/frame.png"]
                          ["../images/frame.jpg" "images/frame.jpg"]
                          ["./frame.webp" "captures/frame.webp"]
                          ["images/space%20name.png" "captures/images/space name.png"]
                          ["images/plus+name.png" "captures/images/plus+name.png"]
                          ["images/plus%2Bname.png" "captures/images/plus+name.png"]
                          ["images%2Fframe.png" "captures/images/frame.png"]
                          ["images/%CE%BB.png" "captures/images/λ.png"]]]
    (is (= expected (attachments/relative-path "captures/observations.md" url))))
  (doseq [url ["../../outside.png" "/outside.png" "//host/image.png" "https://host/image.png"
               "data:image/png;base64,aaa" "file:///image.png" "image.svg" "movie.webm"
               "images/a.png?track=yes" "images/a.png#fragment" "images/%00a.png" "images/%5cprivate.png"
               "images/%zz.png" "images/%FF.png" "images/%ED%A0%80.png" "images//a.png"]]
    (is (nil? (attachments/relative-path "captures/observations.md" url)))))

(deftest dependencies-are-bounded-and-url-projections-require-known-content
  (let [id #uuid "0490e6e3-a857-48ae-a47f-d78fd328628e"
        analysis {:nodes [{:kind :Paragraph :children [{:kind :Image :url "frame.png"}]}]}
        entries {"captures/frame.png" {:content id :size 1 :mode 33188}}]
    (is (= [{:url "frame.png" :path "captures/frame.png"}]
           (attachments/images "captures/source.md" analysis)))
    (is (= {} (attachments/urls (str id) "captures/source.md" analysis {} id)))
    (is (= {} (attachments/urls (str id) "captures/source.md" analysis {"captures/frame.png" nil} id)))
    (is (= 1 (count (attachments/urls (str id) "captures/source.md" analysis entries id))))
    (is (= attachments/max-images
           (count (attachments/images "source.md" {:nodes (mapv #(hash-map :kind :Image :url (str % ".png")) (range 100))}))))))
