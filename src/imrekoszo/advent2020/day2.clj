(ns imrekoszo.advent2020.day2
  (:require
    [clojure.java.io :as io]
    [net.cgrand.xforms :as x]))

(def demo-input
  ["1-3 a: abcde"
   "1-3 b: cdefg"
   "2-9 c: ccccccccc"])

(def re #"(\d+)-(\d+) (\w): (.+)$")

(defn parts [policy+password]
  (let [[_ a b [char & _] password]
        (re-matches re policy+password)]
    [(Long/parseLong a)
     (Long/parseLong b)
     char
     password]))

(defn valid-password?-1 [policy+password]
  (let [[lo hi char password] (parts policy+password)
        char-count (x/count (keep #{char}) password)]
    (<= lo char-count hi)))

(defn valid-password-count-1 [input]
  (x/count (filter valid-password?-1) input))

(defonce input1
  (->> "day2-input1.txt"
    io/resource
    io/reader
    line-seq))

(comment
  (valid-password-count-1 demo-input)
  ;;=> 2

  ;; part 1
  (valid-password-count-1 input1)
  ;;=> 628
  )

(defn valid-password?-2 [policy+password]
  (let [[index1 index2 char password] (parts policy+password)
        char-at #(nth password (dec %))]
    (->> [index1 index2]
      (x/count (comp (map char-at) (keep #{char})))
      (= 1))))

(defn valid-password-count-2 [input]
  (x/count (filter valid-password?-2) input))

(comment
  (valid-password-count-2 demo-input)
  ;;=> 1

  ;; part
  (valid-password-count-2 input1)
  ;;=> 705
  )
