(ns is.simm.documents.links-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.links :as links]))

(def room "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def repository "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def block "cccccccc-cccc-4ccc-8ccc-cccccccccccc")

(deftest route-parser-never-borrows-an-external-host-or-fragment
  (let [ref {:kind :memory-file :room room :path "wiki/A+B.md"} route (links/route ref)]
    (is (= ref (links/parse-route route)))
    (doseq [bad [(str "https://another.example" route) (str "/" route) (str route "#anchor")
                 (str route "&path=other.md") (str route "&permission=owner")]]
      (is (nil? (links/parse-route bad))))))

(deftest file-and-block-links-round-trip-special-paths-in-both-runtimes
  (doseq [path ["wiki/Note.md" "wiki/A B+C%#&=?.md" "研究/🦋.md" "notes/a!'()*~.md"
               (apply str (repeat 512 "研"))]
          ref [{:kind :memory-file :room room :path path}
               {:kind :memory-block :room room :repository repository :block block :path path}]]
    (let [route (links/route ref) query (second (str/split route #"\?" 2))]
      (is (str/starts-with? route (str "/room/" room "/files?")))
      (is (= ref (links/parse-query room query)) (pr-str route))))
  (is (= {:kind :memory-block :room room :repository repository :block block}
         (links/parse-query room (str "repo=" repository "&block=" block)))))

(deftest links-have-one-unambiguous-bounded-interpretation
  (doseq [query ["" "path=a.md&path=b.md" "path=a.md&world=parent" "path="
                 "path=%FF.md" "path=%C0%AF.md" "path=%E2%28%A1.md" "path=%00.md"
                 "path=%ED%A0%80.md" "path=%ED%B0%80.md" "path=%F0%9F%A6.md" "path=%80.md"
                 "path=../secret.md" "path=%2Fsecret.md" "path=a%5Cb.md" "path=a//b.md"
                 "path=a.md&" "path=a%" "path=a%2" "path=a%GG" "path=a.md&&"
                 "repo=x&block=y" (str "block=" block) (str "repo=" repository "&block=" block "&repo=" repository)
                 (str "repo=" repository "&block=" block "&path=")
                 (str "path=" (apply str (repeat 513 "a")))]]
    (is (nil? (links/parse-query room query)) (pr-str query)))
  (is (nil? (links/parse-query "not-a-room" "path=a.md")))
  (is (nil? (links/route {:kind :memory-file :room room :path "a\nb.md"}))))

(deftest raw-unpaired-surrogates-never-become-another-filename
  (doseq [code [0xd800 0xdc00]]
    (let [bad #?(:clj (str (char code)) :cljs (js/String.fromCharCode code))]
      (is (not (links/path? bad)))
      (is (nil? (links/route {:kind :memory-file :room room :path bad})))
      (is (nil? (links/parse-query room (str "path=" bad)))))))

(deftest generated-link-encoding-is-identical-and-aliases-are-safe
  (is (= (str "/room/" room "/files?path=a%20b%2Bc%25%23%26%3D%3F%21%27%28%29*~.md")
         (links/route {:kind :memory-file :room room :path "a b+c%#&=?!'()*~.md"})))
  (is (= {:kind :memory-file :room room :path "a b/c.md"}
         (links/parse-query room "path=a+b%2fc.md")))
  (is (= {:kind :memory-file :room room :path "研究/a=b?.md"}
         (links/parse-query room "p%61th=研究/a=b?.md"))))
