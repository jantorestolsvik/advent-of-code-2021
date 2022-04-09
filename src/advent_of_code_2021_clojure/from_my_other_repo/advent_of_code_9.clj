(ns dev.jan.advent-of-code.advent-of-code-9
  (:require
    [breyta.utils.interface :as u]
    [clojure.edn :as edn]
    [clojure.set :as set]
    [clojure.string :as str]))


;; Day 9
;; Task 1
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input9.txt"))
      lines (vec (map (fn [line] (vec (map edn/read-string (str/split line #"")))) lines))]
  (reduce + (map #(+ % 1) (loop [y 0
                                 low-points []]
                            (if (= y (count lines))
                              low-points
                              (let [row (get lines y)]
                                (recur (+ 1 y) (concat low-points
                                                       (loop [x 0
                                                              low-points []]
                                                         (if (= x (count row))
                                                           low-points
                                                           (recur (+ 1 x)
                                                                  (if (and
                                                                        (or (nil? (get (get lines (- y 1)) x)) (< (get row x) (get (get lines (- y 1)) x)))
                                                                        (or (nil? (get (get lines (+ y 1)) x)) (< (get row x) (get (get lines (+ y 1)) x)))
                                                                        (or (nil? (get row (- x 1))) (< (get row x) (get row (- x 1))))
                                                                        (or (nil? (get row (+ x 1))) (< (get row x) (get row (+ x 1)))))
                                                                    (conj low-points (get row x))
                                                                    low-points))))))))))))


;; Task 2
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input9.txt"))
      lines (vec (map (fn [line] (vec (map edn/read-string (str/split line #"")))) lines))
      basins (loop [y 0
                    basins []]
               (if (= y (count lines))
                 basins
                 (let [row (get lines y)]
                   (recur (+ 1 y) (concat basins
                                          (loop [x 0
                                                 basins []]
                                            (if (= x (count row))
                                              basins
                                              (recur (+ 1 x)
                                                     (if (and
                                                           (or (nil? (get (get lines (- y 1)) x)) (< (get row x) (get (get lines (- y 1)) x)))
                                                           (or (nil? (get (get lines (+ y 1)) x)) (< (get row x) (get (get lines (+ y 1)) x)))
                                                           (or (nil? (get row (- x 1))) (< (get row x) (get row (- x 1))))
                                                           (or (nil? (get row (+ x 1))) (< (get row x) (get row (+ x 1)))))
                                                       (conj basins [#{[x y]} [[(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]]])
                                                       basins)))))))))]
  (reduce * (take 3 (sort > (map count (map (fn [basin]
                                              (loop [[explored [first & rest]] basin]
                                                (let [[x y] first
                                                      value (get-in lines [y x])]
                                                  (if first
                                                    (if (or (nil? value) (= value 9))
                                                      (recur [explored rest])
                                                      (recur [(conj explored first) (concat
                                                                                      rest
                                                                                      (filter (fn [future] (not (contains? explored future))) [[(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]]))]))
                                                    explored)))) basins))))))


(conj #{1, 2} 3)

(some (fn [set] (contains? set [1, 4])) [#{[1, 2] [1, 3]}])

(get [1, 2] 11)
