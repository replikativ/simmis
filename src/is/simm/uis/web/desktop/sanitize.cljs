(ns is.simm.uis.web.desktop.sanitize
  "Final HTML/SVG boundary for untrusted rendered text, not executable widgets."
  (:require ["dompurify" :as DOMPurify]))

(defn- clean [source options]
  ;; DOMPurify can deliberately no-op on unsupported DOM implementations.
  ;; Refuse instead of injecting its original unsanitized input in that case.
  (when-not (true? (.-isSupported ^js DOMPurify))
    (throw (js/Error. "Safe HTML rendering is unavailable in this browser")))
  (.sanitize ^js DOMPurify source options))

(defn html [source]
  (clean source #js {:USE_PROFILES #js {:html true}
                    :FORBID_TAGS #js ["style" "form" "input" "button" "textarea" "select"]
                    :FORBID_ATTR #js ["style"]}))

(defn svg [source]
  ;; Mermaid uses text labels, not HTML foreignObject labels. Never add HTML
  ;; tags to this profile: they cross an SVG/HTML parser boundary on insertion.
  ;; Retain renderer styles for diagram compatibility. This is XSS hardening,
  ;; NOT CSS/network isolation or an arbitrary SVG-upload viewer.
  (clean source #js {:USE_PROFILES #js {:svg true :svgFilters true}
                    :FORBID_TAGS #js ["foreignObject"]}))
