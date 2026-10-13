(ns is.simm.uis.web.desktop.book-period
  "Reporting periods for a team's book, as epoch-millisecond bounds.

   Pure and platform-free (no js/Date, no java.time) for the reason
   `desktop.money` is: the view namespaces cannot load outside a browser, so a
   period computed there could not be asserted, and an income statement over
   the wrong dates is wrong in a way nobody reading it can see.

   Calendar arithmetic is UTC. The book's effective dates are UTC midnights,
   so a period cut in local time would move an entry dated the 1st into the
   previous month for anyone west of Greenwich.")

(def presets
  "`[key label]` in the order the view offers them."
  [[:month "This month"]
   [:quarter "This quarter"]
   [:ytd "Year to date"]
   [:last-year "Last year"]
   [:all "All time"]])

(def default-preset :ytd)

(def ^:private day-ms (* 24 60 60 1000))

(defn- days-from-civil
  "Days since 1970-01-01 for a proleptic Gregorian y-m-d (H. Hinnant)."
  [y m d]
  (let [y (if (<= m 2) (dec y) y)
        era (quot (if (>= y 0) y (- y 399)) 400)
        yoe (- y (* era 400))
        mp (mod (+ m 9) 12)
        doy (+ (quot (+ (* 153 mp) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn- civil-from-days
  "`[y m d]` for days since 1970-01-01 — the inverse of `days-from-civil`."
  [z]
  (let [z (+ z 719468)
        era (quot (if (>= z 0) z (- z 146096)) 146097)
        doe (- z (* era 146097))
        yoe (quot (- doe (quot doe 1460) (- (quot doe 36524)) (quot doe 146096)) 365)
        doy (- doe (- (+ (* 365 yoe) (quot yoe 4)) (quot yoe 100)))
        mp (quot (+ (* 5 doy) 2) 153)
        d (inc (- doy (quot (+ (* 153 mp) 2) 5)))
        m (if (< mp 10) (+ mp 3) (- mp 9))
        y (+ yoe (* era 400))]
    [(if (<= m 2) (inc y) y) m d]))

(defn date-ms
  "Epoch ms of UTC midnight on y-m-d."
  [y m d]
  (* day-ms (days-from-civil y m d)))

(defn ymd
  "`[y m d]` of the UTC day containing `ms`."
  [ms]
  (civil-from-days (#?(:clj Math/floorDiv :cljs (fn [a b] (Math/floor (/ a b)))) ms day-ms)))

(defn period
  "`{:from ms-or-nil :through ms}` for `preset` as seen at `now-ms`. Both are
   UTC midnights: `:through` names a DAY the report includes whole, so the
   bounds stay equal from one render to the next instead of moving with the
   clock. `:from` nil means since the first entry."
  [preset now-ms]
  (let [[y m d] (ymd now-ms)
        today (date-ms y m d)]
    (case preset
      :month {:from (date-ms y m 1) :through today}
      :quarter {:from (date-ms y (inc (* 3 (quot (dec m) 3))) 1) :through today}
      :last-year {:from (date-ms (dec y) 1 1) :through (date-ms (dec y) 12 31)}
      :all {:from nil :through today}
      ;; :ytd and anything unknown
      {:from (date-ms y 1 1) :through today})))

(defn format-day
  "`2026-10-13` for the UTC day containing `ms` — the book's own dates, so no
   locale and no timezone shift."
  [ms]
  (when ms
    (let [[y m d] (ymd ms)
          pad #(if (< % 10) (str "0" %) (str %))]
      (str y "-" (pad m) "-" (pad d)))))
