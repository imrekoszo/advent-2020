(ns imrekoszo.advent2020.day1-alternatives
  (:require
    [clojure.math.combinatorics :as combo]
    [imrekoszo.advent2020.day1
     :as day1
     :refer [part1 part2]
     :rename {part1 part1-xfor
              part2 part2-xfor}]
    [net.cgrand.xforms :as x]))

(def matching-combination-of-2? (day1/matching-combination?-fn 2 false))
(def matching-combination-of-3? (day1/matching-combination?-fn 3 false))
(def matching-combination-of-2?-vec (day1/matching-combination?-fn 2 true))
(def matching-combination-of-3?-vec (day1/matching-combination?-fn 3 true))
(defn product-of-2 [[a b]]
  (* a b))
(defn product-of-3 [[a b c]]
  (* a b c))

;; Alternative 1: just use for (lazy)

(defn part1-for
  {:test (day1/test-part1 part1-for)}
  ([] (part1-for @day1/live-input*))
  ([input]
   (->> (for [a input
              b input
              :when (matching-combination-of-2? a b)]
          (* a b))
     (x/some (filter identity)))))

(defn part2-for
  {:test (day1/test-part2 part2-for)}
  ([] (part2-for @day1/live-input*))
  ([input]
   (->> (for [a input
              b input
              c input
              :when (matching-combination-of-3? a b c)]
          (* a b c))
     (x/some (filter identity)))))

;; Alternative 2: use combinations (lazy)

(defn calculate-combinations [input entry-count matching-combination? product-of]
  (->> entry-count
    (combo/combinations input)
    (x/some (filter matching-combination?))
    (product-of)))

(defn part1-combinations
  {:test (day1/test-part1 part1-combinations)}
  ([] (part1-combinations @day1/live-input*))
  ([input]
   (calculate-combinations input 2 matching-combination-of-2?-vec product-of-2)))

(defn part2-combinations
  {:test (day1/test-part2 part2-combinations)}
  ([] (part2-combinations @day1/live-input*))
  ([input]
   (calculate-combinations input 3 matching-combination-of-3?-vec product-of-3)))

;; Alternative 3: use index-combinations (own algo, needs lots of optimization :( )

(defn test-index-combinations-fn [index-combinations]
  (assert
    (= []
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
    (= [[1 2] [1 3] [2 3] [1 4] [2 4] [3 4]]
      ;; TODO: using x/into here throws weird error
      (into [] (index-combinations 2) [1 2 3 4])))
  (assert
    (= [[1 2 3]
        [1 2 4]
        [1 3 4]
        [2 3 4]
        [1 2 5]
        [1 3 5]
        [2 3 5]
        [1 4 5]
        [2 4 5]
        [3 4 5]]
      (into [] (index-combinations 3) [1 2 3 4 5]))))

(defn calculate-index-combinations [input entry-count matching-combination? product-of index-combinations-xf]
  (->> input
    (x/some
      (comp
        (index-combinations-xf entry-count)
        (filter matching-combination?)))
    (product-of)))

(defn index-combinations
  {:test (test-index-combinations-fn index-combinations)}
  [n]
  (cond
    (< n 1)
    (fn [rf]
      (fn
        ([] (rf))
        ([result] result)
        ([result _] result)))

    (= 1 n)
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input] (rf result [input]))))

    :else
    (fn [rf]
      (let [vvs (volatile! [])]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input]
           (let [prev-vs @vvs
                 _       (vswap! vvs conj input)
                 vs      @vvs
                 cvs     (count vs)]
             (cond
               (< n cvs)
               (transduce
                 (comp
                   (index-combinations (dec n))
                   (map #(conj % input)))
                 rf
                 result
                 prev-vs)

               (= n cvs)
               (rf result vs)

               :else
               result))))))))

(defn part1-index-combinations
  {:test (day1/test-part1 part1-index-combinations)}
  ([] (part1-index-combinations @day1/live-input*))
  ([input]
   (calculate-index-combinations input 2 matching-combination-of-2?-vec product-of-2 index-combinations)))

(defn part2-index-combinations
  {:test (day1/test-part2 part2-index-combinations)}
  ([] (part2-index-combinations @day1/live-input*))
  ([input]
   (calculate-index-combinations input 3 matching-combination-of-3?-vec product-of-3 index-combinations)))

;; Alternative 4: like index-combinations but uses fewer colls inside, leaving partitioning to the caller

(defn append-xf [value]
  {:test
   #(assert (= [0 1 2 3 :foo]
              (into [] (append-xf :foo) (range 4))))}
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result value))
      ([result input] (rf result input)))))

(defn index-combinations-2
  {:test
   (let [sut #(comp (index-combinations-2 %) (x/partition %))]
     (test-index-combinations-fn sut))}
  [n]
  (cond
    (< n 1)
    (fn [rf]
      (fn
        ([] (rf))
        ([result] result)
        ([result _] result)))

    (= 1 n)
    identity

    :else
    (fn [rf]
      (let [vvs (volatile! [])]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input]
           (let [prev-vs @vvs
                 _       (vswap! vvs conj input)
                 vs      @vvs
                 cvs     (count vs)]
             (cond
               (< n cvs)
               (transduce
                 (comp
                   (index-combinations-2 (dec n))
                   (x/partition (dec n) (append-xf input)))
                 rf
                 result
                 prev-vs)

               (= n cvs)
               (reduce rf result vs)

               :else
               result))))))))

(defn part1-index-combinations-2
  {:test (day1/test-part1 part1-index-combinations-2)}
  ([] (part1-index-combinations-2 @day1/live-input*))
  ([input]
   (calculate-index-combinations input 2 matching-combination-of-2?-vec product-of-2 #(comp (index-combinations-2 %) (x/partition %)))))

(defn part2-index-combinations-2
  {:test (day1/test-part2 part2-index-combinations-2)}
  ([] (part2-index-combinations-2 @day1/live-input*))
  ([input]
   (calculate-index-combinations input 3 matching-combination-of-3?-vec product-of-3 #(comp (index-combinations-2 %) (x/partition %)))))

(comment
  (require '[criterium.core :refer [bench quick-bench]])

  (bench (part2-for)) ;; ~169ms
  (bench (part2-xfor)) ;; ~128ms
  (bench (part2-combinations)) ;; ~664ms
  (bench (part2-index-combinations)) ;; ~183ms
  (bench (part2-index-combinations-2)) ;; ~183ms
  )
