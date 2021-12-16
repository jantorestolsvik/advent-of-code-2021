(ns advent-of-code-2021-clojure.day14
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [advent-of-code-2021-clojure.utils :as u]
    [clojure.edn :as edn]))

(defn parse-mapping [fold]
  (let [[a b] (str/split fold #" -> ")
        from (seq (char-array a))
        char (.charAt b 0)
        ]
    [from [(first from) char] [char (second from)]]
    )
  )

(defn map-first-rest [coll]
  (reduce (fn [result [first & rest]] (assoc result first rest)) {} coll)
  )

(defn update-default []
  (assoc map key (get map key 0))
  )

(defn update-in-default ([m k d f & more]
                         (println m k d f more)
                         (assoc m k (apply f (get m k d) more)))
  )

(defn reduce-sum [coll]
  (reduce (fn [result [key number]]
            (update result key (fnil + 0 0) number)) {} coll)
  )

(defn filter-first [f coll]
  (filter (fn [e] (f (first e))) coll)
  )

(defn filter-second [f coll]
  (filter (fn [e] (f (second e))) coll)
  )

(defn divide-by [denominator numerator] (/ numerator denominator))

(defn spread-first [coll]
  (mapcat (fn [[a & rest]] (map (fn [element] (concat [element] rest)) a)) coll)
  )

(defn last-first [coll]
  (apply - ((juxt last first) coll))
  )

(defn task1 [input times] (let [lines (str/split-lines input)
                                [[template] [_ & recepies]] (split-with (partial not= "") lines)
                                start (->> template char-array seq (partition 2 1) frequencies)
                                mapping (->> recepies (map parse-mapping) map-first-rest)
                                initial-freq (->> (nth (iterate
                                                         #(->> %
                                                               (mapcat (fn [[source count]] (map (fn [destination] [destination count]) (get mapping source))))
                                                               reduce-sum
                                                               )
                                                         start)
                                                       times)
                                                  spread-first
                                                  reduce-sum
                                                  (sort-by second)
                                                  (into {})
                                                  )
                                ]
                            (-> initial-freq
                                (update (last template) inc)
                                (update (first template) inc)
                                )

                            ))

(->> (task1 (slurp "resources/input14.txt") 40)
     ;spread-first
     ;reduce-sum
     ;(sort-by second)
     ;(into {})

     (map second)
     (map (partial divide-by 2))
     (sort)
     last-first
     ;(Math/ceil)
     ;(bigint)

     ;(map (partial Math/ceil))
     ;(filter-first #{\B})
     ;(map second)
     ;(apply +)
     ;(Math/ceil)
     ;(int)
     )


(->> (re-seq #"\w+" "HV -> B\nKS -> F") (partition 2) (map vec) (into {}))