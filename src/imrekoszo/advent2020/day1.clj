(ns imrekoszo.advent2020.day1
  (:require
    [clojure.math.combinatorics :as combo]
    [imrekoszo.advent2020.util :as u]
    [net.cgrand.xforms :as x]))

(def parse-input
  (u/parse-input-fn #(map u/parse-long %)))

(defn calculate [input entry-count]
  (->> entry-count
    (combo/combinations input)
    (x/some (filter #(= 2020 (apply + %))))
    (apply *)))

(defn part1
  {:test #(assert (= 514579 (part1 (parse-input "1/demo.txt"))))}
  [input]
  (calculate input 2))

(defn part2
  {:test #(assert (= 241861950 (part2 (parse-input "1/demo.txt"))))}
  [input]
  (calculate input 3))

(comment
  (part1 (parse-input "1/full.txt"))
  ;;=> 436404

  (part2 (parse-input "1/full.txt"))
  ;;=> 274879808
  )
