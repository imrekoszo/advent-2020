(ns imrekoszo.advent2020.day2
  (:require
    [imrekoszo.advent2020.util :as u]
    [net.cgrand.xforms :as x]))

(defn parse-line [policy+password]
  (let [[_ a b [char & _] password]
        (re-matches #"(\d+)-(\d+) (.): (.+)$" policy+password)]
    [(u/parse-long a) (u/parse-long b) char password]))

(def load-input! (u/parse-input-fn #(map parse-line %)))

(def test-input* (delay (load-input! "2/test.txt")))
(def live-input* (delay (load-input! "2/input.txt")))

(defn valid?-1 [[lo hi char password]]
  (let [matching-chars-in-xf (keep #{char})]
    (<= lo (x/count matching-chars-in-xf password) hi)))

(defn valid?-2 [[index1 index2 char password]]
  (let [char-at? #(= char (nth password (dec %)))]
    (not= (char-at? index1) (char-at? index2))))

(defn calculate [input valid?]
  (x/count (filter valid?) input))

(defn part1
  {:test #(assert (= 2 (part1 @test-input*)))}
  ([] (part1 @live-input*))
  ([input]
   (calculate input valid?-1)))

(defn part2
  {:test #(assert (= 1 (part2 @test-input*)))}
  ([] (part2 @live-input*))
  ([input]
   (calculate input valid?-2)))

(comment
  (part1) ;;=> 628
  (part2) ;;=> 705
  )
