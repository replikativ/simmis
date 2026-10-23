(ns is.simm.documents.chart-scene
  "Pure fixed SVG/table scene for a HOST-validated chart DTO. No user-provided
   element names, attributes, styles, markup, URLs or executable expressions."
  (:require [clojure.string :as str]))

(def ^:private colors ["#83b9cf" "#c5a3d3" "#a9c88b" "#dab17d"
                       "#c18f9d" "#8dbdb0" "#a0a6d6" "#d1c482"])
(defn- node [tag attrs children] {:tag tag :attrs attrs :children (vec children)})
(defn- text [x y value]
  (node :text {:x x :y y :fill "currentColor" :font-size 11} [(str value)]))

(defn- domain [values zero?]
  (let [lo (reduce min values) hi (reduce max values)
        lo (if zero? (min 0 lo) lo) hi (if zero? (max 0 hi) hi)]
    (if (= lo hi)
      (let [padding (max 1 (* 0.05 (#?(:clj Math/abs :cljs js/Math.abs) (double lo))))]
        [(- lo padding) (+ hi padding)])
      [lo hi])))

(defn- scale [[lo hi] start end value]
  ;; Coordinates are bounded at decode. Divide before multiplying so tiny or
  ;; large-but-valid domains never overflow the pixel calculation.
  (+ start (* (/ (- value lo) (- hi lo)) (- end start))))

(defn scene [{:keys [kind title x-label y-label series]}]
  (let [points (mapcat :points series)
        categories (when (= :bar kind) (vec (distinct (map first points))))
        indexes (zipmap categories (range))
        xd (when-not categories (domain (map first points) false))
        yd (domain (map second points) (= :bar kind))
        x (if categories #(+ 56 (* (/ (+ 0.5 (get indexes %)) (count categories)) 560))
              #(scale xd 56 616 %))
        y #(scale yd 264 28 %)
        heading (or title "Chart")
        marks (mapcat
               (fn [index {:keys [points]}]
                 (let [color (nth colors index)]
                   (case kind
                     :line (cond-> [(node :path {:d (str/join " " (map-indexed
                                                            (fn [i [a b]] (str (if (zero? i) "M" "L") (x a) "," (y b))) points))
                                         :fill "none" :stroke color :stroke-width 2} [])]
                             (= 1 (count points))
                             (conj (node :circle {:cx (x (ffirst points)) :cy (y (second (first points)))
                                                  :r 3 :fill color} [])))
                     :point (map (fn [[a b]] (node :circle {:cx (x a) :cy (y b) :r 3 :fill color} [])) points)
                     :bar (let [band (/ 560 (count categories)) width (/ (* band 0.8) (count series))]
                            (map (fn [[a b]]
                                   (node :rect {:x (+ (- (x a) (* band 0.4)) (* index width))
                                                :y (min (y 0) (y b)) :width width
                                                :height (#?(:clj Math/abs :cljs js/Math.abs) (double (- (y b) (y 0))))
                                                :fill color} [])) points)))))
               (range) series)
        axes [(node :path {:d "M56,28 V264 H616" :fill "none" :stroke "currentColor" :opacity 0.4} [])
              (text 4 32 (second yd)) (text 4 264 (first yd))
              (text 56 290 (or x-label (if categories "Category" "x")))
              (text 56 16 (or y-label "y"))]
        rows (for [{:keys [label points]} series [a b] points]
               (node :tr {} [(node :td {} [label]) (node :td {} [(str a)]) (node :td {} [(str b)])]))]
    (node :figure {:class "memory-chart"}
          [(node :figcaption {} [heading])
           (node :svg {:viewBox "0 0 640 304" :role "img" :aria-label heading :class "memory-chart-plot"}
                 (concat axes marks))
           (node :ul {:class "memory-chart-legend"}
                 (map-indexed (fn [index {:keys [label]}]
                                (node :li {} [(node :svg {:viewBox "0 0 12 12" :width 12 :height 12 :aria-hidden "true"}
                                                         [(node :circle {:cx 6 :cy 6 :r 4 :fill (nth colors index)} [])])
                                              label])) series))
           (node :p {:class "memory-note"}
                 [(str (count series) " series · " (count points) " points · " (name kind))])
           (node :details {}
                 [(node :summary {} ["Chart data"])
                  (node :table {} [(node :caption {} [heading])
                                  (node :thead {} [(node :tr {} [(node :th {:scope "col"} ["Series"])
                                                                (node :th {:scope "col"} [(or x-label "x")])
                                                                (node :th {:scope "col"} [(or y-label "y")])])])
                                  (node :tbody {} rows)])])])))
