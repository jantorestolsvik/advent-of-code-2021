(ns advent-of-code-2021-clojure.day8
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [advent-of-code-2021-clojure.utils :as u]))


;; Day 8
;; Task 1
(def unique-lengths #{2 3 4 7})
(defn task1 [input] (let [lines (str/split-lines input)]
                      (->> lines
                           (map (fn [line] (str/split line #" \| ")))
                           (map (fn [[input output]]
                                  [(str/split input #" ")
                                   (str/split output #" ")]))
                           (mapcat second)
                           (filter (comp unique-lengths count))
                           count
                           )
                      ))


;; Task 2
;(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input8.txt"))
;      lines (map (fn [line] (str/split line #" \| ")) lines)
;      lines (map (fn [[input output]]
;                   [(str/split input #" ")
;                    (str/split output #" ")]) lines)]
;  (reduce + (map
;              (fn [[input output]]
;                (let
;                  [posibilities [#{"a" "b" "c" "d" "e" "f" "g"}
;                                 #{"a" "b" "c" "d" "e" "f" "g"}
;                                 #{"a" "b" "c" "d" "e" "f" "g"}
;                                 #{"a" "b" "c" "d" "e" "f" "g"}
;                                 #{"a" "b" "c" "d" "e" "f" "g"}
;                                 #{"a" "b" "c" "d" "e" "f" "g"}
;                                 #{"a" "b" "c" "d" "e" "f" "g"}]
;                   values (sort (fn [a b] (- (count a) (count b))) (concat input output))
;                   posibilities (loop
;                                  [posibilities posibilities
;                                   values values]
;                                  (if values
;                                    (let [value (first values)
;                                          set (set (str/split value #""))
;                                          length (count value)]
;                                      (cond
;                                        (= length 2) (recur
;                                                       [(get posibilities 0)
;                                                        (set/intersection set (get posibilities 1))
;                                                        (set/intersection set (get posibilities 2))
;                                                        (get posibilities 3)
;                                                        (get posibilities 4)
;                                                        (get posibilities 5)
;                                                        (get posibilities 6)]
;                                                       (next values))
;                                        (= length 3) (recur
;                                                       [(set/intersection set (get posibilities 0))
;                                                        (set/intersection set (get posibilities 1))
;                                                        (set/intersection set (get posibilities 2))
;                                                        (get posibilities 3)
;                                                        (get posibilities 4)
;                                                        (get posibilities 5)
;                                                        (get posibilities 6)]
;                                                       (next values))
;                                        (= length 4) (recur
;                                                       [(get posibilities 0)
;                                                        (set/intersection set (get posibilities 1))
;                                                        (set/intersection set (get posibilities 2))
;                                                        (get posibilities 3)
;                                                        (get posibilities 4)
;                                                        (set/intersection set (get posibilities 5))
;                                                        (set/intersection set (get posibilities 6))]
;                                                       (next values))
;                                        (= length 5) (recur
;                                                       [(set/intersection set (get posibilities 0))
;                                                        (get posibilities 1)
;                                                        (get posibilities 2)
;                                                        (set/intersection set (get posibilities 3))
;                                                        (get posibilities 4)
;                                                        (get posibilities 5)
;                                                        (set/intersection set (get posibilities 6))]
;                                                       (next values))
;                                        (= length 6) (recur
;                                                       [(set/intersection set (get posibilities 0))
;                                                        (get posibilities 1)
;                                                        (set/intersection set (get posibilities 2))
;                                                        (set/intersection set (get posibilities 3))
;                                                        (get posibilities 4)
;                                                        (set/intersection set (get posibilities 5))
;                                                        (get posibilities 6)]
;                                                       (next values))
;                                        :else (recur
;                                                [(get posibilities 0)
;                                                 (get posibilities 1)
;                                                 (get posibilities 2)
;                                                 (get posibilities 3)
;                                                 (get posibilities 4)
;                                                 (get posibilities 5)
;                                                 (get posibilities 6)]
;                                                (next values))))
;                                    posibilities))
;                   posibilities (loop [posibilities posibilities]
;                                  (if (every? (fn [pos] (= 1 (count pos))) posibilities)
;                                    posibilities
;                                    (recur [(apply set/difference (get posibilities 0) (keep-indexed (fn [index value]
;                                                                                                       (if (and (not= index 0) (= 1 (count value)))
;                                                                                                         value
;                                                                                                         nil)) posibilities))
;                                            (apply set/difference (get posibilities 1) (keep-indexed (fn [index value]
;                                                                                                       (if (and (not= index 1) (= 1 (count value)))
;                                                                                                         value
;                                                                                                         nil)) posibilities))
;                                            (apply set/difference (get posibilities 2) (keep-indexed (fn [index value]
;                                                                                                       (if (and (not= index 2) (= 1 (count value)))
;                                                                                                         value
;                                                                                                         nil)) posibilities))
;                                            (apply set/difference (get posibilities 3) (keep-indexed (fn [index value]
;                                                                                                       (if (and (not= index 3) (= 1 (count value)))
;                                                                                                         value
;                                                                                                         nil)) posibilities))
;                                            (apply set/difference (get posibilities 4) (keep-indexed (fn [index value]
;                                                                                                       (if (and (not= index 4) (= 1 (count value)))
;                                                                                                         value
;                                                                                                         nil)) posibilities))
;                                            (apply set/difference (get posibilities 5) (keep-indexed (fn [index value]
;                                                                                                       (if (and (not= index 5) (= 1 (count value)))
;                                                                                                         value
;                                                                                                         nil)) posibilities))
;                                            (apply set/difference (get posibilities 6) (keep-indexed (fn [index value]
;                                                                                                       (if (and (not= index 6) (= 1 (count value)))
;                                                                                                         value
;                                                                                                         nil)) posibilities))])))
;                   posibilities {(first (get posibilities 0)) 0
;                                 (first (get posibilities 1)) 1
;                                 (first (get posibilities 2)) 2
;                                 (first (get posibilities 3)) 3
;                                 (first (get posibilities 4)) 4
;                                 (first (get posibilities 5)) 5
;                                 (first (get posibilities 6)) 6}
;                   set-to-number {#{0 1 2 3 4 5} "0"
;                                  #{1 2} "1"
;                                  #{0 1 3 4 6} "2"
;                                  #{0 1 2 3 6} "3"
;                                  #{1 2 5 6} "4"
;                                  #{0 2 3 5 6} "5"
;                                  #{0 2 3 4 5 6} "6"
;                                  #{0 1 2} "7"
;                                  #{0 1 2 3 4 5 6} "8"
;                                  #{0 1 2 3 5 6} "9"}]
;                  (Integer/parseInt (str/join (map (fn [letter-number] (get set-to-number (set (map (fn [char] (get posibilities char)) (str/split letter-number #""))))) output)))))
;              lines)))


(every? (fn [pos] (= 1 (count pos))) [#{"e"} #{"f"} #{"c"} #{"d"} #{"a"} #{"b"} #{"g"}])

(filter (comp unique-lengths count) '( "gfcae"
                                       "dfcab"
                                       "cgabdf"
                                       "fgeadb"
                                       "ade"
                                       "dcbge"
                                       "gcbfed"
                                       "cfbeg"
                                       "gdfecba"
                                       "gecbd"
                                       "bdc"
                                       "cdb"))
