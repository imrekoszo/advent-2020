(ns imrekoszo.advent2020.day1
  (:require
    [clojure.math.combinatorics :as combo]
    [imrekoszo.advent2020.util :as u]
    [net.cgrand.xforms :as x]))

(def load-input! (u/parse-input-fn #(map u/parse-long %)))
(def test-input* (delay (load-input! "1/test.txt")))
(def live-input* (delay (load-input! "1/input.txt")))

(defn matching-combination?
  [xs]
  (= 2020 (apply + xs)))

(defn test-part1 [f]
  (fn [] (assert (= 514579 (f @test-input*)))))

(defn test-part2 [f]
  (fn [] (assert (= 241861950 (f @test-input*)))))

;; Approach 1 just use for (lazy)

(defn part1-for
  {:test (test-part1 part1-for)}
  ([] (part1-for @live-input*))
  ([input]
   (->> (for [a input
              b input
              :when (matching-combination? [a b])]
          (* a b))
     (x/some (filter identity)))))

(defn part2-for
  {:test (test-part2 part2-for)}
  ([] (part2-for @live-input*))
  ([input]
   (->> (for [a input
              b input
              c input
              :when (matching-combination? [a b c])]
          (* a b c))
     (x/some (filter identity)))))

;; Approach 2 use x/for (eduction)

(defn part1-xfor
  {:test (test-part1 part1-xfor)}
  ([] (part1-xfor @live-input*))
  ([input]
   (->> (x/for [a input
                b input
                :when (matching-combination? [a b])]
          (* a b))
     (x/some (filter identity)))))

(defn part2-xfor
  {:test (test-part2 part2-xfor)}
  ([] (part2-xfor @live-input*))
  ([input]
   (->> (x/for [a input
                b input
                c input
                :when (matching-combination? [a b c])]
          (* a b c))
     (x/some (filter identity)))))

;; Approach 3 use combinations (lazy)

(defn calculate-combinations [input entry-count]
  (->> entry-count
    (combo/combinations input)
    (x/some (filter matching-combination?))
    (apply *)))

(defn part1-combinations
  {:test (test-part1 part1-combinations)}
  ([] (part1-combinations @live-input*))
  ([input]
   (calculate-combinations input 2)))

(defn part2-combinations
  {:test (test-part2 part2-combinations)}
  ([] (part2-combinations @live-input*))
  ([input]
   (calculate-combinations input 3)))

;; Approach 4 use index-combinations (own algo, needs lots of optimization :( )

(defn index-combinations
  {:test
   #(do
      (assert
        (= [[]]
          (into [] (index-combinations -1) [1 2 3])
          (into [] (index-combinations 0) [1 2 3])
          (into [] (index-combinations 4) [1 2 3])))
      (assert
        (= [[1] [2] [3] [4]]
          (into [] (index-combinations 1) [1 2 3 4])))
      (assert
        (= [[1 2 3]]
          (into [] (index-combinations 3) [1 2 3])))
      (assert
        (= [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]
          (into [] (index-combinations 2) [1 2 3 4])))
      (assert
        (= [[1 2 3]
            [1 2 4]
            [1 2 5]
            [1 3 4]
            [1 3 5]
            [1 4 5]
            [2 3 4]
            [2 3 5]
            [2 4 5]
            [3 4 5]]
          (into [] (index-combinations 3) [1 2 3 4 5]))))}
  [n]
  (fn [rf]
    (let [vs (transient [])]
      (fn
        ([] (rf))
        ([result]
         (let [vs (persistent! vs)
               cv (count vs)]
           (cond
             (or (< n 1) (< cv n))
             (rf result [])

             (= n cv)
             (rf result vs)

             (= 1 n)
             (transduce (map vector) rf result vs)

             :else
             (let [[fv & rvs] vs
                   rn (dec n)]
               (as-> result $
                 (transduce
                   (comp (index-combinations rn)
                     (map #(into [fv] %)))
                   rf $ rvs)
                 (transduce
                   (index-combinations n)
                   rf $ rvs))))))
        ([result input]
         (conj! vs input)
         result)))))

(defn calculate-index-combinations [input entry-count]
  (->> input
    (x/some
      (comp
        (index-combinations entry-count)
        (filter matching-combination?)))
    (apply *)))

(defn part1-index-combinations
  {:test (test-part1 part1-index-combinations)}
  ([] (part1-index-combinations @live-input*))
  ([input]
   (calculate-index-combinations input 2)))

(defn part2-index-combinations
  {:test (test-part2 part2-index-combinations)}
  ([] (part2-index-combinations @live-input*))
  ([input]
   (calculate-index-combinations input 3)))

;; select fastest one
(def part1 part1-xfor)
(def part2 part2-xfor)

(comment
  (part1) ;;=> 436404
  (part2) ;;=> 274879808

  (time (part2-for)) ;; 600-ish ms
  (time (part2-xfor)) ;; 170-180-ish ms
  (time (part2-combinations)) ;; 130-ish ms
  (time (part2-index-combinations)) ;; 1200-ish ms

  )
