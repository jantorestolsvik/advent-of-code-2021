(ns advent-of-code-2021-clojure.day13
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [advent-of-code-2021-clojure.utils :as u]
    [clojure.edn :as edn]))

(defn parse-fold [fold]
  (let [[first second] (str/split fold #"=")]
    [(last first) (edn/read-string second)]
    )
  )
(parse-fold "fold along y=7")

(defn print-map [coords]
  (let [max-x (apply max (map first coords))
        max-y (apply max (map second coords))]
    (for [y (range (inc max-y))]
      (println (str/join (for [x (range (inc max-x))]
                           (if (contains? coords [x y])
                             "█"
                             ".")
                           )))
      )
    ))

(defn fold [input [char coord]]
  (if (= char \y)
    (map (fn [[x y]] [x (if (> y coord) (- coord (- y coord)) y)]) input)
    (map (fn [[x y]] [(if (> x coord) (- coord (- x coord)) x) y]) input)))

(defn task1 [input] (let [lines (str/split-lines input)]
                      (->> lines
                           (split-with (partial not= ""))
                           ((juxt #(-> % first u/lines->points) #(->> % second rest (map parse-fold) (take 1))))
                           (apply (partial reduce fold))
                           set
                           count
                           )
                      ))

(defn task2 [input] (let [lines (str/split-lines input)]
                      (->> lines
                           (split-with (partial not= ""))
                           ((juxt #(-> % first u/lines->points) #(->> % second rest (map parse-fold))))
                           (apply (partial reduce fold))
                           set
                           print-map
                           )
                      ))
