(ns aoc2018.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse [input]
  (-> (io/resource input)
      (slurp)
      (string/split #"\n")
      (->> (map #(Integer/parseInt %)))))

(defn part1 [freqs]
  (->> freqs
       (reduce +)))

(defn part2 [freqs]
  (->> (cycle freqs)
       (reduce (fn [[freq known] df]
                 (let [current (+ freq df)]
                   (if (known current)
                     (reduced current)
                     [current (conj known current)])))
               [0 #{}])))

(comment
  (def freqs
    (parse "day-1-input"))

  (part1 freqs)
  (part2 freqs)
  )
