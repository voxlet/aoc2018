(ns aoc2018.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn reactive? [a b]
  (and (not= a b)
       (= (string/upper-case a)
          (string/upper-case b))))

(defn react [polymer]
  (loop [done []
         left (rest polymer)
         unit (first polymer)]
    (if (empty? left)
      done
      (let [other (first left)]
        (if (reactive? unit other)
          (if (empty? done)
            (recur done (rest left) other)
            (recur (pop done) (rest left) (peek done)))
          (recur (conj done unit) (rest left) other))))))

(defn part1 [polymer]
  (count (react (seq polymer))))

(defn snip [polymer t]
  (remove #(= (string/lower-case %) t) polymer))

(defn possible-types [polymer]
  (distinct (map string/lower-case polymer)))

(defn part2 [polymer]
  (->> (possible-types polymer)
       (mapv #(-> polymer seq (snip %) react count))
       (apply min)))

(comment
  (def polymer
    (-> "day-5-input" io/resource slurp))

  (part1 polymer)

  (part2 polymer)
  )
