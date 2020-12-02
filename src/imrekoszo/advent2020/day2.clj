(ns imrekoszo.advent2020.day2
  (:require [clojure.java.io :as io]))

(def demo-input
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"])

(def re #"^(?<min>\d+)-(?<max>\d+) (?<char>\w): (?<password>.+)$")

(defn xcount [xform coll]
  (transduce xform (completing (fn [acc _] (inc acc))) 0 coll))

(defn valid-password? [policy+password]
  (let [[_ lo hi [char & _] password]
        (re-matches re policy+password)]
    (<= (Long/parseLong lo)
        (xcount (keep #{char}) password)
        (Long/parseLong hi))))

(defn valid-password-count [input]
  (xcount (filter valid-password?) input))

(defonce input1
  (->> "day2-input1.txt"
       io/resource
       io/reader
       line-seq))

(comment

  (valid-password-count demo-input)

  ;; part 1
  (valid-password-count input1)
  ;;=> 628




  )
