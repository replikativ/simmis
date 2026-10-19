(ns is.simm.uis.web.desktop.markdown-security-test
  (:require [cljs.test :refer-macros [deftest is]]
            ["dompurify" :as DOMPurify]
            [is.simm.uis.web.desktop.markdown :as markdown]
            [is.simm.uis.web.desktop.sanitize :as sanitize]
            [is.simm.uis.web.desktop.views.core :as views]
            [is.simm.uis.web.desktop.views.mermaid :as mermaid]))

(deftest unaccepted-proposal-blocks-are-sanitized-at-the-render-boundary
  (let [vnode (views/static-block
               "<p>Proposed text</p><img src='data:,' onerror='void 0'><a href='javascript:void(0)'>Link</a>"
               "diff-add")
        ^js el (js/document.createElement "div")]
    (set! (.-innerHTML el) (:innerHTML (:attrs vnode)))
    (is (= "proposal-block diff-add" (:class (:attrs vnode))))
    (is (= "Proposed text" (.-textContent (.querySelector el "p"))))
    (is (nil? (.querySelector el "[onerror], a[href]"))))
  (is (= "" (:innerHTML (:attrs (views/static-block nil "diff-remove"))))))

(deftest legacy-code-extraction-sanitizes-before-parsing
  (is (= "(str \"<safe>\")"
         (sanitize/code-text
          "<img src='data:,' onerror='void 0'><pre><code>(str &quot;&lt;safe&gt;&quot;)</code></pre>")))
  (is (nil? (sanitize/code-text nil)))
  (is (nil? (sanitize/code-text "<p>No code</p>")))
  (let [supported (.-isSupported ^js DOMPurify)]
    (try
      (set! (.-isSupported ^js DOMPurify) false)
      (is (thrown? js/Error (sanitize/code-text "<code>refuse unsafe parsing</code>")))
      (finally (set! (.-isSupported ^js DOMPurify) supported)))))

(deftest untrusted-markdown-cannot-insert-event-handlers-or-active-html
  (let [^js el (markdown/create-markdown-element
                (str "<img src='data:,' onerror='void 0'>"
                     "<script>void 0</script>"
                     "<form><input name='password'><button>Submit</button></form>"
                     "<div style='position:fixed'>Readable text</div>"
                     "<svg onload='void 0'><text>SVG is not Markdown HTML</text></svg>"))]
    (is (nil? (.querySelector el "[onerror], [onload], script, form, input, button, svg, [style]")))
    (is (.includes (.-textContent el) "Readable text"))))

(deftest untrusted-links-cannot-use-executable-protocols
  (doseq [source ["[unsafe](javascript:void(0))"
                  "<a href='JaVaScRiPt:void(0)'>unsafe</a>"
                  "<a href='jav&#x61;script:void(0)'>unsafe</a>"
                  "<a href='data:text/html,unsafe'>unsafe</a>"]]
    (let [^js el (markdown/create-markdown-element source)]
      (is (nil? (.querySelector el "a[href]")) source))))

(deftest ordinary-markdown-and-highlighted-code-remain-readable
  (let [^js el (markdown/create-markdown-element
                (str "# Title\n\n[Safe](https://example.com/docs)\n\n"
                     "| A | B |\n|---|---|\n| 1 | 2 |\n\n"
                     "```clojure\n(defn greeting [] \"Hello\")\n```"))]
    (is (= "Title" (.-textContent (.querySelector el "h1"))))
    (is (= "https://example.com/docs" (.getAttribute (.querySelector el "a") "href")))
    (is (some? (.querySelector el "table")))
    (is (some? (.querySelector el "pre code.hljs")))
    (is (.includes (.-textContent el) "defn greeting"))))

(deftest svg-profile-keeps-diagrams-without-active-or-foreign-html
  (let [^js el (js/document.createElement "div")]
    (set! (.-innerHTML el)
          (sanitize/svg
           (str "<svg xmlns='http://www.w3.org/2000/svg' onload='void 0' viewBox='0 0 100 30'>"
                "<g class='node'><rect width='100' height='30' onclick='void 0'/><text>Safe label</text></g>"
                "<a href='javascript:void(0)'><text>Unsafe link</text></a>"
                "<foreignObject><div xmlns='http://www.w3.org/1999/xhtml' onclick='void 0'>HTML</div></foreignObject>"
                "<script>void 0</script></svg>")))
    (is (some? (.querySelector el "svg g.node rect")))
    (is (.includes (.-textContent el) "Safe label"))
    (is (nil? (.querySelector el "[onload], [onclick], script, foreignObject, div, a[href]")))))

(deftest svg-style-retention-is-not-a-css-isolation-promise
  (let [^js el (js/document.createElement "div")]
    (set! (.-innerHTML el) (sanitize/svg "<svg><style>.node { fill: red; }</style><rect class='node'/></svg>"))
    (is (some? (.querySelector el "svg style")))
    (is (some? (.querySelector el "svg rect.node")))))

(deftest unsupported-dom-sanitization-fails-closed
  (let [supported (.-isSupported ^js DOMPurify)]
    (try
      (set! (.-isSupported ^js DOMPurify) false)
      (is (thrown? js/Error (sanitize/html "<img onerror='void 0'>")))
      (is (thrown? js/Error (sanitize/svg "<svg onload='void 0'/>")))
      (finally (set! (.-isSupported ^js DOMPurify) supported)))))

(deftest diagram-directives-cannot-override-site-security-or-label-policy
  (let [config mermaid/configuration secure (set (:secure config))]
    (is (= "strict" (:securityLevel config)))
    (is (false? (:htmlLabels config)))
    (is (false? (get-in config [:flowchart :htmlLabels])))
    (is (every? secure ["secure" "securityLevel" "startOnLoad" "maxTextSize"
                        "suppressErrorRendering" "maxEdges" "htmlLabels" "flowchart" "themeCSS"]))))
