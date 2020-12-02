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

(defn first-match [coll]
  (->> coll
    (x/some (filter #(= 2020 (apply + %))))
    (apply *)))

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
