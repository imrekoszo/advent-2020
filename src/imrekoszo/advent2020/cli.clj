(ns imrekoszo.advent2020.cli
  (:require
    [clojure.string :as str]
    [imrekoszo.advent2020.util :as u]
    [net.cgrand.xforms :as x]))

(def day-ns-prefix
  (-> *ns*
    (ns-name)
    (name)
    (str/split #"\.")
    (butlast)
    (concat ["day"])
    (->> (str/join \.))))

(defn safe-get-part-fn [day part]
  (try
    @(requiring-resolve (symbol (str day-ns-prefix day "/part" part)))
    (catch Exception _)))

(defn run-day+part! [[day part]]
  (when-let [part-fn (safe-get-part-fn day part)]
    (print (format "Day %2d, part %d:" day part))
    (println (format " %16d" (part-fn)))))

(defn try-parse-long [s]
  (when (string? s)
    (try (u/parse-long s) (catch Exception _))))

(def first-day 1)
(def last-day 24)

(defn ->days
  {:test
   #(do
      (assert (= nil (->days {})))
      (assert (= [1] (->days {:a 1})))
      (assert (= [1] (->days {:a 1 :b 2})))
      (assert (= (range 1 25) (->days {:a 1 :range? true})))
      (assert (= [1 2] (->days {:a 1 :range? true :b 2})))
      (assert (= nil (->days {:b 1})))
      (assert (= nil (->days {:range? true})))
      (assert (= [1 2 3 4 5] (->days {:range? true :b 5})))
      (assert (= (range 1 25) (->days {:a -2 :range? true :b 30})))
      (assert (= nil (->days {:a 28 :range? true :b 30}))))}
  [{:keys [a range? b]}]
  (->>
    (if a
      (if range?
        (range a (inc (or b last-day)))
        [a])
      (when (and b range?)
        (range first-day (inc b))))
    (filter #(<= first-day % last-day))
    (seq)))

(defn parse-day-spec [day-spec]
  (when (string? day-spec)
    (-> day-spec
      (->> (re-matches #"(\d+)?(-)?(\d+)?"))
      (rest)
      (->> (zipmap [:a :range? :b]))
      (update :a try-parse-long)
      (update :range? boolean)
      (update :b try-parse-long))))

(def day-specs->days-xf
  (comp
    (map parse-day-spec)
    (map ->days)
    cat
    (distinct)
    (x/sort)))

(defn days-to-run
  {:test
   #(do
      (assert (= (range 1 25)
                (days-to-run nil)
                (days-to-run [])
                (days-to-run ["asdf" 1])))
      (assert (= [5] (days-to-run ["5"])))
      (assert (= [2 6] (days-to-run ["6" "2"])))
      (assert (= [1 2 3 7 10 15 16 17 22 23 24]
                (days-to-run ["asdf" 123 "22-" "-3" "10" "7" "15-17"]))))}
  [day-specs]
  (or (seq (eduction day-specs->days-xf day-specs))
    (range 1 25)))

(defn -main [& day-specs]
  (run!
    run-day+part!
    (x/for [day  (days-to-run day-specs)
            part (->> 2 (inc) (range 1))]
      [day part])))
