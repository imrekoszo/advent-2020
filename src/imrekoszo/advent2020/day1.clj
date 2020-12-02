(ns imrekoszo.advent2020.day1
  (:require
    [clojure.java.io :as io]
    [net.cgrand.xforms :as x]))

(defonce input1
  (->> "day1-input1.txt"
    (io/resource)
    (io/reader)
    (line-seq)
    (map #(Integer/parseInt %))))

(def first-match
  (partial transduce
    (comp
      (filter #(= 2020 (apply + %)))
      (map #(apply * %)))
    (completing #(reduced %2))
    nil))

(defn result1 []
  (first-match (x/for [a input1 b input1] [a b])))

(defn result2 []
  (first-match (x/for [a input1 b input1 c input1] [a b c])))

(comment
  (result1)
  ;;=> 436404

  (result2)
  ;;=> 274879808
  )
