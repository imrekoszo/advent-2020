(ns imrekoszo.advent2020.day1
  (:require
    [clojure.math.combinatorics :as combo]
    [imrekoszo.advent2020.io :as iio]
    [net.cgrand.xforms :as x]))

(def demo-input
  [1721
   979
   366
   299
   675
   1456])

(defn calculate* [input entry-count]
  (->> entry-count
    (combo/combinations input)
    (x/some (filter #(= 2020 (apply + %))))
    (apply *)))

(defn calculate1 [input]
  (calculate* input 2))

(defn calculate2 [input]
  (calculate* input 3))

(def full-input
  (->> "day1.txt"
    (iio/input-seq)
    (map #(Integer/parseInt %))))

(comment
  ;; part 1
  (calculate1 demo-input)
  ;;=> 514579
  (calculate1 full-input)
  ;;=> 436404

  ;; part 2
  (calculate2 demo-input)
  ;;=> 241861950
  (calculate2 full-input)
  ;;=> 274879808
  )
