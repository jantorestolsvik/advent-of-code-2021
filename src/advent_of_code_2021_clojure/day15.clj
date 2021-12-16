(ns advent-of-code-2021-clojure.day15
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [advent-of-code-2021-clojure.utils :as u]
    [clojure.edn :as edn]
    [advent-of-code-2021-clojure.dijkstra :as dijkstra]))

(defn shortest-path-for-costs [costs] (let [
                                            cost-map (into {} (reduce concat (map-indexed (fn [x list] (map-indexed (fn [y val] [[x y] (into {} (u/filter-second (complement nil?) {[(+ x 1) y] (get-in costs [(+ x 1) y])
                                                                                                                                                                                    [(- x 1) y] (get-in costs [(- x 1) y])
                                                                                                                                                                                    [x (+ y 1)] (get-in costs [x (+ y 1)])
                                                                                                                                                                                    [x (- y 1)] (get-in costs [x (- y 1)])}))]) list)) costs)))
                                            cost-map-result (dijkstra/dijkstra [0 0] cost-map)
                                            ]
                                        cost-map
                                        (get cost-map-result [(- (count costs) 1) (- (count (get costs 0)) 1)])
                                        ))

(defn task1 [input]
  (->> input
       (str/split-lines)
       (mapv (comp vec (partial map edn/read-string) (partial re-seq #"\d")))
       shortest-path-for-costs
       )
  )
(defn inc-wrap [max wrap number]
  (let [inced (inc number)]
    (if (> inced max)
      wrap
      inced)
    )
  )

(defn task2 [input]
  (->> input
       (str/split-lines)
       (mapv (comp vec (partial map edn/read-string) (partial re-seq #"\d")))
       (map #(->> %
                  (iterate (partial map (partial inc-wrap 9 1)))
                  (take 5)
                  (reduce concat)))
       (iterate (partial map (partial map (partial inc-wrap 9 1))))
       (take 5)
       (reduce concat)
       (map vec)
       (vec)
       shortest-path-for-costs
       )
  )

(task1 (slurp "resources/input15.txt"))
(task2 (slurp "resources/input15.txt"))

(take 4 (iterate inc 1))