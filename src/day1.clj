(ns day1
  (:require [clojure.java.io :as io]))

(defn sums-to-2020 [a b]
  (= 2020 (+ a b)))

(defonce input1
  (->> "day1-input1.txt"
       io/resource
       io/reader
       line-seq
       (map #(Integer/parseInt %))))

(defn result1 []
  (->> (for [a input1 b input1] [a b])
       (reduce
        (fn [_ [a b]]
          (when (sums-to-2020 a b) (reduced (* a b))))
        nil)))

(comment
  (result1)
  ;;=> 436404

  )
