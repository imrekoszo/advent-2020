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
