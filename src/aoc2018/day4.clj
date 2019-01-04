(ns aoc2018.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-guards [lines]
  (-> (reduce (fn [guards line]
                (let [timestamp #"\[(.*?) 00:(\d+)\]"]
                  (cond
                    (string/includes? line "Guard")
                    (let [[_ id] (re-find #"Guard #(\d+)" line)]
                      (assoc guards ::state {:id id}))

                    (string/includes? line "falls asleep")
                    (let [[_ date minute] (re-find timestamp line)]
                      (update guards ::state merge
                              {:date date
                               :sleep-minute (Integer/parseInt minute)}))

                    (string/includes? line "wakes up")
                    (let [[_ _ wake-minute] (re-find timestamp line)
                          {:keys [id date sleep-minute]} (::state guards)]
                      (update-in guards [id :sleeps date] (fnil into [])
                                 (range sleep-minute
                                        (Integer/parseInt wake-minute)))))))
              {} lines)
      (dissoc ::state)))

(defn parse [input]
  (-> (io/resource input)
      (slurp)
      (string/split-lines)
      (sort)
      (parse-guards)))

(defn all-minutes [guard]
  (apply concat (-> guard :sleeps vals)))

(defn max-sleep-minute [guard]
  (-> guard all-minutes frequencies
      (->> (apply max-key val))))

(defn part1 [guards]
  (let [sleepy-guard (apply max-key #(-> % val all-minutes count) guards)]
    (-> sleepy-guard val max-sleep-minute key
        (* (-> sleepy-guard key (Integer/parseInt))))))

(defn part2 [guards]
  (let [[id [m _]] (->> guards
                        (mapv (fn [[id g]]
                                [id (max-sleep-minute g)]))
                        (apply max-key #(-> % second val)))]
    (* (Integer/parseInt id) m)))

(comment
  (def guards
    (parse "day-4-input"))

  (part1 guards)

  (part2 guards)
  )

