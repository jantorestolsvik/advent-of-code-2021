(ns advent-of-code-2021-clojure.day11
  (:require
    [clojure.edn :as edn]
    [clojure.set :as set]
    [clojure.string :as str]))


;; Day 10
;; Task 1
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input11.txt"))
      lines (vec (map (fn [line] (vec (map edn/read-string (str/split line #"")))) lines))
      ]
  (loop [i 0
         flashes 0
         lines lines
         ]
    (if (= 100 i)
      [flashes lines]
      (let [[count next-map] (let [lines-plus-one (mapv (fn [line] (mapv inc line)) lines)
                                   [count result] (loop [fired #{}
                                          current lines-plus-one
                                          [first & rest] (reduce set/union (map-indexed (fn [x line] (keep-indexed (fn [y value] (if (> value 9) [x y] nil)) line)) lines-plus-one))
                                          ]
                                     (if first
                                       (let [[x y] first]
                                         (if (or (contains? fired first) (< (get-in current first) 10))
                                           (recur
                                             fired
                                             current
                                             rest)
                                           (let [neighbours (filter (fn [[x y]] (and
                                                                                  (< x (count current))
                                                                                  (< y (count (nth current 0)))
                                                                                  (> x -1)
                                                                                  (> y -1)
                                                                                  )) [
                                                                                      [(+ x 1) y]
                                                                                      [(- x 1) y]
                                                                                      [x (+ y 1)]
                                                                                      [x (- y 1)]
                                                                                      [(+ x 1) (+ y 1)]
                                                                                      [(+ x 1) (- y 1)]
                                                                                      [(- x 1) (+ y 1)]
                                                                                      [(- x 1) (- y 1)]
                                                                                      ])]
                                             (recur
                                               (conj fired first)
                                               (reduce (fn [res neighbour] (update-in res neighbour inc)) current neighbours)
                                               (concat rest neighbours))
                                             )
                                           )
                                         )
                                       [(count fired) current])
                                     )
                                   ]
                               [count (mapv (fn [line] (mapv (fn [val] (if (> val 9)
                                                                  0
                                                                  val)) line))
                                     result)]
                               )] (recur (inc i) (+ flashes count) next-map)))
    )
  )

;; Task 2
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input11.txt"))
      lines (vec (map (fn [line] (vec (map edn/read-string (str/split line #"")))) lines))
      ]
  (loop [i 0
         flashes 0
         lines lines
         ]
    (if (every? (fn [line] (every? #(= % 1) line)) lines)
      [(dec i) flashes lines]
      (let [[count next-map] (let [lines-plus-one (mapv (fn [line] (mapv inc line)) lines)
                                   [count result] (loop [fired #{}
                                          current lines-plus-one
                                          [first & rest] (reduce set/union (map-indexed (fn [x line] (keep-indexed (fn [y value] (if (> value 9) [x y] nil)) line)) lines-plus-one))
                                          ]
                                     (if first
                                       (let [[x y] first]
                                         (if (or (contains? fired first) (< (get-in current first) 10))
                                           (recur
                                             fired
                                             current
                                             rest)
                                           (let [neighbours (filter (fn [[x y]] (and
                                                                                  (< x (count current))
                                                                                  (< y (count (nth current 0)))
                                                                                  (> x -1)
                                                                                  (> y -1)
                                                                                  )) [
                                                                                      [(+ x 1) y]
                                                                                      [(- x 1) y]
                                                                                      [x (+ y 1)]
                                                                                      [x (- y 1)]
                                                                                      [(+ x 1) (+ y 1)]
                                                                                      [(+ x 1) (- y 1)]
                                                                                      [(- x 1) (+ y 1)]
                                                                                      [(- x 1) (- y 1)]
                                                                                      ])]
                                             (recur
                                               (conj fired first)
                                               (reduce (fn [res neighbour] (update-in res neighbour inc)) current neighbours)
                                               (concat rest neighbours))
                                             )
                                           )
                                         )
                                       [(count fired) current])
                                     )
                                   ]
                               [count (mapv (fn [line] (mapv (fn [val] (if (> val 9)
                                                                  0
                                                                  val)) line))
                                     result)]
                               )] (recur (inc i) (+ flashes count) next-map)))
    )
  )

(update-in [[1]] [0 5] (fnil inc 0))

(nth [3] 0)

(for [a (range 10)]
  a
  )