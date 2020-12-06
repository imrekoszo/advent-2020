(ns imrekoszo.advent2020.day1
  (:require
    [imrekoszo.advent2020.util :as u]
    [net.cgrand.xforms :as x]))

(def load-input! (u/parse-input-fn #(map u/parse-long %)))
(def test-input* (delay (load-input! "1/test.txt")))
(def live-input* (delay (load-input! "1/input.txt")))

(defmacro matching-combination?-fn [n vec?]
  (let [syms (mapv #(symbol (str "x" %)) (range n))]
    `(fn ~(if vec? `[[~@syms]] `[~@syms]) (= 2020 (+ ~@syms)))))

(def matching-combination-of-2? (matching-combination?-fn 2 false))
(def matching-combination-of-3? (matching-combination?-fn 3 false))

(defn test-part1 [f]
  (fn [] (assert (= 514579 (f @test-input*)))))

(defn test-part2 [f]
  (fn [] (assert (= 241861950 (f @test-input*)))))

;; I am not particularly satisfied with these solutions
;; Although they are better than using clojure.core/for (which returns a
;; lazy sequence as opposed to the eduction returned by x/for),
;; there are still 2 things I don't like:
;;
;; 1/ Like for, x/for cannot be parameterized dynamically, so I can't say
;;    give me the cartesian product of n times the same set. Which is why I
;;    had to write the x/for twice. (Didn't want to macro it)
;; 2/ This task doesn't need a cartesian product, but n-tuples assembled from
;;    the items at n-combinations of incides within the input.
;;
;; Tried some alternative implementations in the namespace
;; imrekoszo.advent2020.day1-alternatives but they all appear to be slower than
;; this one.

(defn part1
  {:test (test-part1 part1)}
  ([] (part1 @live-input*))
  ([input]
   (->> (x/for [a input
                b input
                :when (matching-combination-of-2? a b)]
          (* a b))
     (x/some (filter identity)))))

(defn part2
  {:test (test-part2 part2)}
  ([] (part2 @live-input*))
  ([input]
   (->> (x/for [a input
                b input
                c input
                :when (matching-combination-of-3? a b c)]
          (* a b c))
     (x/some (filter identity)))))

(comment
  (part1) ;;=> 436404
  (part2) ;;=> 274879808
  )
