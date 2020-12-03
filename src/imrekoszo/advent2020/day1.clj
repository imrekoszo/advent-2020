(ns imrekoszo.advent2020.day1
  (:require
    [clojure.math.combinatorics :as combo]
    [imrekoszo.advent2020.util :as u]
    [net.cgrand.xforms :as x]))

(def load-input! (u/parse-input-fn #(map u/parse-long %)))

(def test-input* (delay (load-input! "1/test.txt")))
(def live-input* (delay (load-input! "1/input.txt")))

(defn calculate [input entry-count]
  (->> entry-count
    (combo/combinations input)
    (x/some (filter #(= 2020 (apply + %))))
    (apply *)))

(defn part1
  {:test #(assert (= 514579 (part1 @test-input*)))}
  ([] (part1 @live-input*))
  ([input]
   (calculate input 2)))

(defn part2
  {:test #(assert (= 241861950 (part2 @test-input*)))}
  ([] (part2 @live-input*))
  ([input]
   (calculate input 3)))

(comment
  (part1) ;;=> 436404
  (part2) ;;=> 274879808
  )
