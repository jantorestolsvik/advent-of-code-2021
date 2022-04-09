(ns dev.jan.advent-of-code.advent-of-code-12
  (:require
    [breyta.utils.interface :as u]
    [clojure.edn :as edn]
    [clojure.set :as set]
    [clojure.string :as str]))


;; Day 12
;; Task 1
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input12.txt"))
      lines (mapv (fn [line] (vec (str/split line #"-"))) lines)
      graph (reduce (fn [map [start end]]
                      (-> map
                          (update start set/union #{end})
                          (update end set/union #{start}))) {} lines)]
  (loop [paths [["start"]]]
    (let [paths-without-end (filter (fn [path] (not= (last path) "end")) paths)
          paths-with-end (filter (fn [path] (= (last path) "end")) paths)]
      (if (= (count paths-without-end) 0)
        (count paths)
        (recur (concat
                 paths-with-end
                 (mapcat
                   (fn [path]
                     (keep
                       (fn [future-node]
                         (if
                           (and
                             (not= future-node "start")
                             (not (and
                                    (re-matches #"[a-z]+" future-node)
                                    (some #{future-node} path)
                                    (some
                                      (fn [[name frequency]] (> frequency 1))
                                      (frequencies (filter (fn [node] (re-matches #"[a-z]+" node)) path))))))
                           (conj path future-node)
                           nil))
                       (get graph (last path))))
                   paths-without-end)))))))


(re-matches #"[a-z]+" "A")
(some #{3} [1 2 3])
