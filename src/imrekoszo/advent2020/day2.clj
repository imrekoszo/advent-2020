(ns imrekoszo.advent2020.day2
  (:require
    [imrekoszo.advent2020.io :as iio]
    [net.cgrand.xforms :as x]))

(defn parse-line [policy+password]
  (let [[_ a b [char & _] password]
        (re-matches #"(\d+)-(\d+) (\w): (.+)$" policy+password)]
    [(Long/parseLong a) (Long/parseLong b) char password]))

(def demo-input
  (mapv parse-line
    ["1-3 a: abcde"
     "1-3 b: cdefg"
     "2-9 c: ccccccccc"]))

(defn valid-by-rule-1? [[lo hi char password]]
  (let [matching-chars-in (keep #{char})]
    (<= lo (x/count matching-chars-in password) hi)))

(defn valid-by-rule-2? [[index1 index2 char password]]
  (let [char-at           #(nth password (dec %))
        matching-chars-at (comp (map char-at) (keep #{char}))]
    (= 1 (x/count matching-chars-at [index1 index2]))))

(defn calculate* [input valid?]
  (x/count (filter valid?) input))

(defn calculate1 [input]
  (calculate* input valid-by-rule-1?))

(defn calculate2 [input]
  (calculate* input valid-by-rule-2?))

(defonce full-input
  (->> "day2.txt"
    (iio/input-seq)
    (map parse-line)))

(comment
  ;; part 1
  (calculate1 demo-input)
  ;;=> 2
  (calculate1 full-input)
  ;;=> 628

  ;; part 2
  (calculate2 demo-input)
  ;;=> 1
  (calculate2 full-input)
  ;;=> 705
  )
