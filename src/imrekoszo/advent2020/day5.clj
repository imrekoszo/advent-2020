(ns imrekoszo.advent2020.day5
  (:require [clojure.string :as str]
            [imrekoszo.advent2020.util :as u]
            [net.cgrand.xforms :as x]))

(defn fbrl->long
  {:test #(assert (= 44 (fbrl->long "FBFBBFF")))}
  [s]
  (-> s
    (str/replace #"F|B|R|L" #(case % ("F" "L") "0" ("B" "R") "1"))
    (Long/parseLong 2)))

(def load-input! (u/parse-input-fn #(map fbrl->long %)))
(def live-input* (delay (load-input! "5/input.txt")))

(defn part1
  ([] (part1 @live-input*))
  ([input]
   (x/some x/max input)))

(comment
  (part1) ;;=> 965
  )
