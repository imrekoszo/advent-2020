(ns imrekoszo.advent2020.day4
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [imrekoszo.advent2020.util :as u]
            [net.cgrand.xforms :as x]))

(def parse-fields-xf
  (comp
    (map #(str/split % #":"))
    (x/for [[k v] %] [(keyword k) v])))

(defn parse-entry [raw-entry]
  (-> raw-entry
    (->> (str/join " "))
    (str/split #" ")
    (->> (into {} parse-fields-xf))))

(def parse-xf
  (comp
    (partition-by empty?)
    (remove #{[""]})
    (map parse-entry)))

(defn parse-input [lines]
  (into [] parse-xf lines))

(def load-input! (u/parse-input-fn parse-input))
(def test-input* (delay (load-input! "4/test.txt")))
(def test2-input* (delay (load-input! "4/test2.txt")))
(def live-input* (delay (load-input! "4/input.txt")))

(s/def :day4.part1/byr any?)
(s/def :day4.part1/iyr any?)
(s/def :day4.part1/eyr any?)
(s/def :day4.part1/hgt any?)
(s/def :day4.part1/hcl any?)
(s/def :day4.part1/ecl any?)
(s/def :day4.part1/pid any?)
(s/def :day4.part1/cid any?)

(s/def :day4.part1/passport
  (s/keys
    :req-un [:day4.part1/byr :day4.part1/iyr :day4.part1/eyr
             :day4.part1/hgt :day4.part1/hcl :day4.part1/ecl :day4.part1/pid]
    :opt-un [:day4.part1/cid]))

(defn calculate [input spec]
  (x/count (filter #(s/valid? spec %)) input))

(defn part1
  {:test
   #(assert (= 2 (part1 @test-input*)))}
  ([] (part1 @live-input*))
  ([input]
   (calculate input :day4.part1/passport)))

(defn string-matching? [re]
  (s/and string? #(re-matches re %)))

(defn int-between [at-least at-most]
  #(<= at-least % at-most))

(defn int-str-between
  ([re at-least at-most]
   (int-str-between re identity at-least at-most))
  ([re pre-parse at-least at-most]
   (s/and
     (string-matching? re)
     #(-> % pre-parse u/parse-long ((int-between at-least at-most))))))

(defn year-between [at-least at-most]
  (int-str-between #"\d{4}" at-least at-most))

(s/def :day4.part2/byr (year-between 1920 2002))
(s/def :day4.part2/iyr (year-between 2010 2020))
(s/def :day4.part2/eyr (year-between 2020 2030))
(s/def :day4.part2/hgt (s/or
                         :cm (int-str-between #"1\d\dcm" #(subs % 0 3) 150 193)
                         :in (int-str-between #"\d\din" #(subs % 0 2) 59 76)))
(s/def :day4.part2/hcl (string-matching? #"#[0-9a-f]{6}"))
(s/def :day4.part2/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def :day4.part2/pid (string-matching? #"\d{9}"))
(s/def :day4.part2/cid :day4.part1/cid)

(s/def :day4.part2/passport
  (s/keys
    :req-un [:day4.part2/byr :day4.part2/iyr :day4.part2/eyr
             :day4.part2/hgt :day4.part2/hcl :day4.part2/ecl :day4.part2/pid]
    :opt-un [:day4.part2/cid]))

(defn part2
  {:test
   #(assert (= 4 (part2 @test2-input*)))}
  ([] (part2 @live-input*))
  ([input]
   (calculate input :day4.part2/passport)))

(comment
  (part1) ;;=> 260
  (part2) ;;=> 153
  )
