(ns dev.jan.advent-of-code.advent-of-code-12-alt
  (:require
    [breyta.utils.interface :as u]
    [clojure.edn :as edn]
    [clojure.set :as set]
    [clojure.string :as str]))


;; Day 12
;; Task 1
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input12_1.txt"))
      lines (mapv (fn [line] (vec (str/split line #"-"))) lines)
      graph (reduce (fn [map [start end]]
                      (-> map
                          (update start set/union #{end})
                          (update end set/union #{start}))) {} lines)]
  (loop [el "start"
         ctr {}]
    (cond
      (= el "end") 1
      (and (= el "start") (> (get ctr el 0) 1)) 0
      :else (get graph el))))


(re-matches #"[a-z]+" "A")
(some #{3} [1 2 3])
