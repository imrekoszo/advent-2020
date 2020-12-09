(ns imrekoszo.advent2020.day3
  (:require
    [imrekoszo.advent2020.util :as u]
    [net.cgrand.xforms :as x]))

(def test-input* (delay (u/input-seq "3/test.txt")))
(def live-input* (delay (u/input-seq "3/input.txt")))

(defn tree-at?
  {:test #(assert
            (= [false false true true false]
              (mapv (partial tree-at? "..##.") (range 5))))}
  [row index]
  (= \# (nth row index)))

(defn calculate
  {:test #(do
            (assert (= 2 (calculate @test-input* 1 1)))
            (assert (= 7 (calculate @test-input* 3 1)))
            (assert (= 3 (calculate @test-input* 5 1)))
            (assert (= 4 (calculate @test-input* 7 1)))
            (assert (= 2 (calculate @test-input* 1 2))))}
  [grid step-right step-down]
  (let [width (count (first grid))]
    (letfn [(col-at-when-in-row [row-index]
              (mod (* row-index step-right) width))
            (trees-encountered [row-index row]
              (or (tree-at? row (col-at-when-in-row row-index)) nil))]
      (x/count
        (comp
          (take-nth step-down)
          (keep-indexed trees-encountered))
        grid))))

(defn part1
  {:test #(assert (= 7 (part1 @test-input*)))}
  ([] (part1 @live-input*))
  ([input]
   (calculate input 3 1)))

(defn part2
  {:test #(assert (= 336 (part2 @test-input*)))}
  ([] (part2 @live-input*))
  ([input]
   (transduce
     (map (partial apply calculate input)) *
     [[1 1] [3 1] [5 1] [7 1] [1 2]])))

(comment
  (part1) ;;=> 145
  (part2) ;;=> 3424528800
  )
