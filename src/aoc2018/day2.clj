(ns aoc2018.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse [input]
  (-> (io/resource input)
      (slurp)
      (string/split #"\n")))

(defn part1 [ids]
  (let [fs (map #(-> % frequencies vals set) ids)
        dup-count (fn [n]
                    (count (filter #(contains? % n) fs)))]
    (* (dup-count 2) (dup-count 3))))

(defn comb-seq [coll]
  (when (pos? (count coll))
    (concat (map (fn [n] [(first coll) n]) (rest coll))
            (comb-seq (rest coll)))))

(defn match [a b]
  (let [common (->> (map (fn [ca cb]
                           (when (= ca cb) ca))
                         a b)
                    (apply str))]
    (when (= 1 (- (count a) (count common)))
      common)))

(defn part2 [ids]
  (->> (comb-seq ids)
       (keep #(apply match %))
       (first)))

(comment
  (def ids
    (parse "day-2-input"))

  (part1 ids)

  (count ids)
  ;; => 250
  (count (comb-seq ids))
  ;; 250C2 = 31125 ...brute force is good enough

  (part2 ids)
  )

