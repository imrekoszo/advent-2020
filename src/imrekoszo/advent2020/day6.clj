(ns imrekoszo.advent2020.day6
  (:require [clojure.set :as set]
            [imrekoszo.advent2020.util :as u]
            [net.cgrand.xforms :as x]))

(defn parse-input [lines]
  (x/into [] u/separate-by-blank-lines-xf lines))

(def load-input! (u/parse-input-fn parse-input))
(def test-input* (delay (load-input! "6/test.txt")))
(def live-input* (delay (load-input! "6/input.txt")))

(defn calculate [grouped-personal-positive-answers count-per-group]
  (transduce
    (map count-per-group) +
    grouped-personal-positive-answers))

(defn part1
  {:test #(assert (= 11 (part1 @test-input*)))}
  ([] (part1 @live-input*))
  ([grouped-personal-positive-answers]
   (calculate
     grouped-personal-positive-answers
     #(x/count (comp cat (distinct)) %))))

(defn part2
  {:test #(assert (= 6 (part2 @test-input*)))}
  ([] (part2 @live-input*))
  ([grouped-personal-positive-answers]
   (calculate
     grouped-personal-positive-answers
     #(count
        (transduce
          (comp (drop 1) (map set))
          (completing
            (fn [questions-everyone-answered-yes-to-so-far
                 questions-answered-yes]
              (let [result (set/intersection
                             questions-everyone-answered-yes-to-so-far
                             questions-answered-yes)]
                (cond-> result (empty? result) (reduced)))))
          (set (first %))
          %)))))

(comment
  (part1) ;;=> 6351
  (part2) ;;=> 3143
  )
