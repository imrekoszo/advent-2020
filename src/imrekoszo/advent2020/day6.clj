(ns imrekoszo.advent2020.day6
  (:require [imrekoszo.advent2020.util :as u]
            [net.cgrand.xforms :as x]))

(defn parse-input [lines]
  (x/into [] u/separate-by-blank-lines-xf lines))

(def load-input! (u/parse-input-fn parse-input))
(def test-input* (delay (load-input! "6/test.txt")))
(def live-input* (delay (load-input! "6/input.txt")))

(def count-group-xf
  (map #(x/count (comp cat (distinct)) %)))

(defn part1
  {:test #(assert (= 11 (part1 @test-input*)))}
  ([] (part1 @live-input*))
  ([groups]
   (transduce count-group-xf + groups)))

(comment
  (part1) ;;=> 6351
  )

