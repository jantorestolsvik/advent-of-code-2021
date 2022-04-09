(ns dev.jan.advent-of-code.advent-of-code-10
  (:require
    [breyta.utils.interface :as u]
    [clojure.edn :as edn]
    [clojure.set :as set]
    [clojure.string :as str]))


;; Day 10
;; Task 1
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input10.txt"))
      lines (vec (map (fn [line] (str/split line #"")) lines))
      match {")" "(" "]" "[" ">" "<" "}" "{"}
      points {")" 3 "]" 57 ">" 25137 "}" 1197}
      results (map (fn [line]
                     (loop [line line
                            prev nil]
                       (if line
                         (let [this (first line)
                               last-open (first prev)]
                           (if (#{"(" "[" "<" "{"} this)
                             (recur (next line) (conj prev this))
                             (if (= (get match this) last-open)
                               (recur (next line) (next prev))
                               this)))
                         (if prev :incomplete :complete)))) lines)]
  (reduce + (filter (complement nil?) (map points results))))


;; Task 2
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input10.txt"))
      lines (vec (map (fn [line] (str/split line #"")) lines))
      match {")" "("
             "(" ")"
             "]" "["
             "[" "]"
             ">" "<"
             "<" ">"
             "}" "{"
             "{" "}"}
      points {")" 1 "]" 2 "}" 3 ">" 4}
      results (keep (fn [line]
                      (loop [line line
                             prev nil]
                        (if line
                          (let [this (first line)
                                last-open (first prev)]
                            (if (#{"(" "[" "<" "{"} this)
                              (recur (next line) (conj prev this))
                              (if (= (get match this) last-open)
                                (recur (next line) (next prev))
                                nil)))
                          prev))) lines)
      a (map (fn [rest]
               (loop [rest rest
                      result []]
                 (if rest
                   (recur (next rest) (conj result (get match (first rest))))
                   result))) results)
      sorted (sort < (map (fn [row] (reduce (fn [result value] (+ (* 5 result) (get points value))) 0 row)) a))]
  ;; sorted
  ;; (- (int (/ (count results) 2)) 1)
  ;; (int (/ (count results) 2))
  ;; 53 -> 26 * 2
  ;; nr 27
  (nth sorted (int (/ (count results) 2)))
  ;; sorted
  )


(int (/ (count '(1 2 3)) 2))

(nth '(1 2 3) 1)

({"a" 1} "a")
