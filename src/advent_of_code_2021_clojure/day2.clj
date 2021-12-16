(ns advent-of-code-2021-clojure.day2
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn task1 [input]
  (->>
    (str/split-lines input)
    (map (fn [line] (str/split line #" ")))
    (map (fn [[op number]] [op (edn/read-string number)]))
    (reduce (fn [result [op number]]
              (cond
                (= op "forward") (update result :horizontal + number)
                (= op "up") (update result :depth - number)
                (= op "down") (update result :depth + number))) {:horizontal 0 :depth 0})
    ((fn [result] (* (:horizontal result) (:depth result)))))
  )

;; Task 2
(defn task2 [input]
  (->>
    (str/split-lines input)
    (map (fn [line] (str/split line #" ")))
    (map (fn [[op number]] [op (edn/read-string number)]))
     (reduce (fn [result [op number]]
              (cond
                (= op "forward") (assoc result
                                   :horizontal (+ (:horizontal result) number)
                                   :depth (+ (:depth result) (* (:aim result) number))
                                   )
                (= op "up") (assoc result
                              :aim (- (:aim result) number)
                              )
                (= op "down") (assoc result
                                :aim (+ (:aim result) number)
                                )
                )
              ) {:horizontal 0 :depth 0 :aim 0})
     ((fn [result] (* (:horizontal result) (:depth result))))
    )
  )