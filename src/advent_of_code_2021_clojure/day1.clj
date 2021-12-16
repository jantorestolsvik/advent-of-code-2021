(ns advent-of-code-2021-clojure.day1
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [advent-of-code-2021-clojure.utils :as u]))

;; 1
;; Task 1
(defn task1 [input]
  (->> input
       (str/split-lines)
       (map edn/read-string)
       (partition 2 1)
       (filter (partial apply <))
       count
       )
  )


;; Task 2
(defn task2 [input]
  (->> input
       (str/split-lines)
       (map edn/read-string)
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (filter (partial apply <))
       count
       )
  )
