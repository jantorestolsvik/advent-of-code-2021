(ns advent-of-code-2021-clojure.utils
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn pprintr
  [data]
  (pprint/pprint data)
  data)

(defn lines->points [lines] (map #(as-> % x (str/split x #",") (map edn/read-string x)) lines))

(map #(as-> % x (str/split x #",") (map edn/read-string x)) '("6,10"
                                "0,14"))

(defn filter-second [f coll]
  (filter (fn [e] (f (second e))) coll)
  )

(as->
  "0,14" x
  (str/split x #",")
  (map edn/read-string x)
  )
((partial map edn/read-string) ["0", "1"])