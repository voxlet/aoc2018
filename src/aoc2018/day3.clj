(ns aoc2018.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [aoc2018.day2 :as day2]))

(defn parse-line [line]
  (zipmap [:id :x :y :w :h]
          (->> (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line)
               (rest)
               (mapv #(Integer/parseInt %)))))

(defn parse [input]
  (-> (io/resource input)
      (slurp)
      (string/split-lines)
      (->> (map parse-line))))

(defn line-intersect [as aw bs bw]
  (let [ae (+ as aw)
        be (+ bs bw)
        s (max as bs)
        w (- (min ae be) s)]
    (when (> w 0)
      [s w])))

(defn intersect [a b]
  (when-let [[x w] (line-intersect (:x a) (:w a) (:x b) (:w b))]
    (when-let [[y h] (line-intersect (:y a) (:h a) (:y b) (:h b))]
      {:x x :y y :w w :h h})))

(comment
  (intersect {:x 3 :y 3 :w 10 :h 5}
             {:x 1 :y 4 :w 15 :h 10}))

(defn intersections [rs]
  (->> rs
       (day2/comb-seq)
       (keep #(apply intersect %))))

(defn contain [{ax :x ay :y :as a} {bx :x by :y :as b}]
  (when (and (<= bx ax (+ ax (:w a)) (+ bx (:w b)))
             (<= by ay (+ ay (:h a)) (+ by (:h b))))
    b))

(defn difference [a b]
  (if (contain a b)
    []
    (if-let [i (intersect a b)]
      (let [left-w (- (:x i) (:x a))
            right-x (+ (:x i) (:w i))
            right-w (- (+ (:x a) (:w a)) right-x)
            bottom-y (+ (:y i) (:h i))
            top-h (- (:y i) (:y a))
            mid-h (:h i)
            bottom-h (- (+ (:y a) (:h a)) bottom-y)]
        (->> [(when (pos? top-h)
                (assoc a :h top-h))
              (when (pos? left-w)
                {:x (:x a) :y (:y i) :w left-w :h mid-h})
              (when (pos? right-w)
                {:x right-x :y (:y i) :w right-w :h mid-h})
              (when (pos? bottom-h)
                (merge a {:y bottom-y :h bottom-h}))]
             (filter identity)))
      [a])))

(defn disjoint-add [disjoint-set r]
  (if-not (seq disjoint-set)
    [r]
    (->> (reduce (fn [drs d]
                   (into [] (mapcat #(difference % d)) drs))
                 [r] disjoint-set)
         (into disjoint-set))))

(defn area [{:keys [w h]}]
  (* w h))

;; go for a geometric solution for added fun
(defn part1 [claims]
  (->> (intersections claims)
       (reduce disjoint-add [])
       (reduce (fn [a r] (+ a (area r))) 0)))

(defn no-overlaps? [r claims]
  (->> claims
       (mapv (fn [other]
               (when-not (= (:id r) (:id other))
                 (intersect r other))))
       (every? nil?)))

(defn part2 [claims]
  (->> claims
       (filter #(no-overlaps? % claims))
       (first)
       :id))

(comment
  (def claims
    (parse "day-3-input"))

  (part1 claims)

  (part2 claims)
  )
