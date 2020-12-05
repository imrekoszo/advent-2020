(ns imrekoszo.advent2020.day4
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [imrekoszo.advent2020.util :as u]
            [net.cgrand.xforms :as x]))

(def parse-raw-fields-xf
  (comp
    (map #(str/split % #":"))
    (x/for [[k v] %] [(keyword k) v])))

(defn parse-entry [raw-entry]
  (-> raw-entry
    (->> (str/join " "))
    (str/split #" ")
    (->> (into {} parse-raw-fields-xf))))

(def parse-xf
  (comp
    (partition-by empty?)
    (remove #(every? empty? %))
    (map parse-entry)))

(defn parse-input [lines]
  (sequence parse-xf lines))

(def load-input! (u/parse-input-fn parse-input))
(def test-input* (delay (load-input! "4/test.txt")))
(def live-input* (delay (load-input! "4/input.txt")))

(s/def ::byr any?)
(s/def ::iyr any?)
(s/def ::eyr any?)
(s/def ::hgt any?)
(s/def ::hcl any?)
(s/def ::ecl any?)
(s/def ::pid any?)
(s/def ::cid any?)

(s/def ::passport
  (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid] :opt-un [::cid]))

(defn part1
  {:test
   #(assert (= 2 (part1 @test-input*)))}
  ([] (part1 @live-input*))
  ([input]
   (x/count (filter #(s/valid? ::passport %)) input)))

(comment
  (part1) ;;=> 260
  )
