(ns dev.jan.advent-of-code.advent-of-code
  (:require
    [breyta.utils.interface :as u]
    [clojure.edn :as edn]
    [clojure.string :as str]))


;; Advent of code

;; 1
;; Task 1
(reduce + (map (fn [[a b]] (if (> b a) 1 0)) (partition 2 1 (map edn/read-string (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input.txt"))))))


;; Task 2
(reduce + (map (fn [[a b]] (if (> b a) 1 0)) (partition 2 1 (map #(reduce + %) (partition 3 1 (map edn/read-string (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input.txt"))))))))


;; 2
;; Task 1
(->>
  (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input2.txt"))
  (map (fn [line] (str/split line #" ")))
  (map (fn [[op number]] [op (edn/read-string number)]))

  (reduce (fn [result [op number]]
            (cond
              (= op "forward") (assoc result :horizontal (+ (:horizontal result) number))
              (= op "up") (assoc result :depth (- (:depth result) number))
              (= op "down") (assoc result :depth (+ (:depth result) number)))) {:horizontal 0 :depth 0})
  ((fn [result] (* (:horizontal result) (:depth result)))))


;; Task 2
(->>
  (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input2.txt"))
  (map (fn [line] (str/split line #" ")))
  (map (fn [[op number]] [op (edn/read-string number)]))
  ;; (reduce (fn [result [op number]]
  ;;          (cond
  ;;            (= op "forward") (assoc result
  ;;                               :horizontal (+ (:horizontal result) number)
  ;;                               :depth (+ (:depth result) (* (:aim result) number))
  ;;                               )
  ;;            (= op "up") (assoc result
  ;;                          :aim (- (:aim result) number)
  ;;                          )
  ;;            (= op "down") (assoc result
  ;;                            :aim (+ (:aim result) number)
  ;;                            )
  ;;            )
  ;;          ) {:horizontal 0 :depth 0 :aim 0})
  ;; ((fn [result] (* (:horizontal result) (:depth result))))
  )


;; 3
;; Task 1

(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input3.txt"))
      sums (->>
             lines
             (map (fn [line] (map edn/read-string (str/split line #""))))
             (map vec)
             (apply mapv +)

             ;; (map (fn [[op number]] [op (edn/read-string number)]))
             ;; (reduce (fn [result [op number]]
             ;;          (cond
             ;;            (= op "forward") (assoc result
             ;;                               :horizontal (+ (:horizontal result) number)
             ;;                               :depth (+ (:depth result) (* (:aim result) number))
             ;;                               )
             ;;            (= op "up") (assoc result
             ;;                          :aim (- (:aim result) number)
             ;;                          )
             ;;            (= op "down") (assoc result
             ;;                            :aim (+ (:aim result) number)
             ;;                            )
             ;;            )
             ;;          ) {:horizontal 0 :depth 0 :aim 0})
             ;; ((fn [result] (* (:horizontal result) (:depth result))))
             )
      length (count lines)
      gamma (map (fn [sum] (if (> sum (/ length 2)) 1 0)) sums)
      epsilon (map (fn [sum] (if (< sum (/ length 2)) 1 0)) sums)]
  (u/pprintr sums)
  (* (Long/parseLong (str/join gamma) 2)
     (Long/parseLong (str/join epsilon) 2)))


;; Task 2
(defn day3task2
  [index oOrc numbers]
  (let [sums (->>
               numbers
               (map vec)
               (apply mapv +))
        length (count numbers)
        oxygen (map (fn [sum] (if (>= sum (/ length 2)) 1 0)) sums)
        co2 (map (fn [sum] (if (< sum (/ length 2)) 1 0)) sums)]
    (if (= oOrc :oxygen)
      (filter (fn [number] (= (nth number index) (nth oxygen index))) numbers)
      (filter (fn [number] (= (nth number index) (nth co2 index))) numbers))))


(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input3.txt"))
      numbers (->>
                lines
                (map (fn [line] (map edn/read-string (str/split line #"")))))
      sums (->>
             numbers
             (map vec)
             (apply mapv +))
      length (count lines)
      oxygen (map (fn [sum] (if (> sum (/ length 2)) 1 0)) sums)
      co2 (map (fn [sum] (if (< sum (/ length 2)) 1 0)) sums)]

  (*
    (Long/parseLong (str/join (nth (->>
                                     numbers
                                     (day3task2 0 :oxygen)
                                     (day3task2 1 :oxygen)
                                     (day3task2 2 :oxygen)
                                     (day3task2 3 :oxygen)
                                     (day3task2 4 :oxygen)
                                     (day3task2 5 :oxygen)
                                     (day3task2 6 :oxygen)
                                     (day3task2 7 :oxygen)
                                     (day3task2 8 :oxygen)
                                     (day3task2 9 :oxygen)
                                     (day3task2 10 :oxygen)
                                     (day3task2 11 :oxygen)) 0)) 2)
    (Long/parseLong (str/join (nth (->>
                                     numbers
                                     (day3task2 0 :co2)
                                     (day3task2 1 :co2)
                                     (day3task2 2 :co2)
                                     (day3task2 3 :co2)
                                     (day3task2 4 :co2)
                                     (day3task2 5 :co2)
                                     (day3task2 6 :co2)
                                     (day3task2 7 :co2)
                                     (day3task2 8 :co2)) 0)) 2)))


;; Day 4
;; Task 1
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input4.txt"))
      [draws-raw & rest] lines
      draws (map edn/read-string (str/split draws-raw #","))
      raw-boards (filter #(not= "" %) rest)
      boards (->>
               raw-boards
               (partition 5)
               (map (fn [partitioned-raw-board] (map (fn [row] (map edn/read-string (filter #(not= "" %) (str/split row #"\s+")))) partitioned-raw-board))))]
  (loop [draws draws
         prev-draw nil
         boards boards
         marks (map (fn [board] (map (fn [row] (map (fn [_] 0) row)) board)) boards)]
    (if (not-any? (fn [board-marks]
                    (or
                      (some (fn [row-marks] (= (reduce + row-marks) 5)) board-marks)
                      (some (fn [col-sum] (= col-sum 5)) (apply map + board-marks)))) marks)
      (recur (next draws)
             (first draws)
             boards
             (map
               (fn [board-marks-a board-marks-b] (map (fn [row-marks-a row-marks-b] (map + row-marks-a row-marks-b)) board-marks-a board-marks-b))
               marks
               (map (fn [board] (map (fn [row] (map (fn [number] (if (= number (first draws)) 1 0)) row)) board)) boards)))

      (let [[index marks] (first (keep-indexed (fn [index board-marks]
                                                 (if (or
                                                       (some (fn [row-marks] (= (reduce + row-marks) 5)) board-marks)
                                                       (some (fn [col-sum] (= col-sum 5)) (apply map + board-marks))) [index board-marks] nil)) marks))]
        [(nth boards index) index marks]
        (*
          prev-draw
          (reduce + (map (fn [row row-marks] (reduce + (map * row (map (fn [mark] (if (= mark 0) 1 0)) row-marks)))) (nth boards index) marks)))))))


;; Task 2
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input4.txt"))
      [draws-raw & rest] lines
      draws (map edn/read-string (str/split draws-raw #","))
      raw-boards (filter #(not= "" %) rest)
      boards (->>
               raw-boards
               (partition 5)
               (map (fn [partitioned-raw-board] (map (fn [row] (map edn/read-string (filter #(not= "" %) (str/split row #"\s+")))) partitioned-raw-board))))]
  (loop [draws draws
         prev-draw nil
         boards boards
         marks (map (fn [board] (map (fn [row] (map (fn [_] 0) row)) board)) boards)]
    (u/pprintr (count boards))
    (if (or
          (> (count boards) 1)
          (not (or
                 (some (fn [row-marks] (= (reduce + row-marks) 5)) (first marks))
                 (some (fn [col-sum] (= col-sum 5)) (apply map + (first marks))))))
      (let [boards-and-marks (map
                               (fn [board board-marks]
                                 (if (or
                                       (some (fn [row-marks] (= (reduce + row-marks) 5)) board-marks)
                                       (some (fn [col-sum] (= col-sum 5)) (apply map + board-marks))) nil [board board-marks]))
                               boards marks)
            filter-value (filter (fn [result] (not (nil? result)))
                                 boards-and-marks)

            [non-finished-boards non-finished-marks]
            (reduce (fn [[a b] [board board-marks]] [(concat a [board]) (concat b [board-marks])]) [[] []] filter-value)]

        (recur (next draws)
               (first draws)
               non-finished-boards
               (map
                 (fn [board-marks-a board-marks-b] (map (fn [row-marks-a row-marks-b] (map + row-marks-a row-marks-b)) board-marks-a board-marks-b))
                 non-finished-marks
                 (map (fn [board] (map (fn [row] (map (fn [number] (if (= number (first draws)) 1 0)) row)) board)) non-finished-boards))))
      (do
        (u/pprintr [boards marks])
        (*
          prev-draw
          (reduce + (map (fn [row row-marks] (reduce + (map * row (map (fn [mark] (if (= mark 0) 1 0)) row-marks)))) (first boards) (first marks))))))))


;; Day 5
;; Task 1
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input5.txt"))
      lines (map (fn [line] (map (fn [point] (map edn/read-string (str/split point #","))) (str/split line #" -> "))) lines)
      lines (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))) lines)
      max-x (apply max (mapcat (fn [[[x1] [x2]]] [x1 x2]) lines))
      max-y (apply max (mapcat (fn [[[_ y1] [_ y2]]] [y1 y2]) lines))
      board (vec (repeat (+ max-x 1) (vec (repeat (+ max-y 1) 0))))]
  (loop [board board
         lines lines]
    (if lines
      (recur
        (loop [board board
               [[x1 y1] [x2 y2]] (first lines)]
          (cond
            (< x1 x2) (recur (assoc-in board [x1 y1] (+ (get-in board [x1 y1]) 1)) [[(+ x1 1) y1] [x2 y2]])
            (> x1 x2) (recur (assoc-in board [x1 y1] (+ (get-in board [x1 y1]) 1)) [[(- x1 1) y1] [x2 y2]])
            (< y1 y2) (recur (assoc-in board [x1 y1] (+ (get-in board [x1 y1]) 1)) [[x1 (+ y1 1)] [x2 y2]])
            (> y1 y2) (recur (assoc-in board [x1 y1] (+ (get-in board [x1 y1]) 1)) [[x1 (- y1 1)] [x2 y2]])
            :else (assoc-in board [x1 y1] (+ (get-in board [x1 y1]) 1))))
        (next lines))
      (do
        (reduce + (map (fn [col] (count (filter (fn [cell] (> cell 1)) col))) board))))))


(defn clamp1
  [value]
  (min (max value -1) 1))


;; Task 2
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input5.txt"))
      lines (map (fn [line] (map (fn [point] (map edn/read-string (str/split point #","))) (str/split line #" -> "))) lines)
      max-x (apply max (mapcat (fn [[[x1] [x2]]] [x1 x2]) lines))
      max-y (apply max (mapcat (fn [[[_ y1] [_ y2]]] [y1 y2]) lines))
      board (vec (repeat (+ max-x 1) (vec (repeat (+ max-y 1) 0))))]
  (loop [board board
         lines lines]
    (if lines
      (recur
        (loop [board board
               [[x1 y1] [x2 y2]] (first lines)]
          (cond
            (and (= x1 x2) (= y1 y2)) (assoc-in board [x1 y1] (+ (get-in board [x1 y1]) 1))
            :else (recur (assoc-in board [x1 y1] (+ (get-in board [x1 y1]) 1)) [[(+ x1 (clamp1 (- x2 x1))) (+ y1 (clamp1 (- y2 y1)))] [x2 y2]])))
        (next lines))
      (do
        (reduce + (map (fn [col] (count (filter (fn [cell] (> cell 1)) col))) board))))))


(map + '(1, 2, 3) '(1, 2, 3))
(map + '(1, 2, 3) '(1, 2, 3))

(assoc-in [[0]] [0 0] (+ (get-in [[0]] [0 0]) 1))

(get-in [[1]] [0 0])


;; Day 6
;; Task 1 and 2
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input6.txt"))
      fish (map (fn [[days fish]] [days (count fish)]) (group-by identity (map edn/read-string (str/split (first lines) #","))))
      fish-count (reduce (fn [result [days day-counts]] (assoc result days (reduce + (map (fn [[day count]] count) day-counts)))) (vec (repeat 9 0)) (group-by (fn [[days fish]] days) fish))]
  (loop [day 1
         fish-count fish-count]
    (if (= day (+ 1 256))
      (reduce + fish-count)
      (recur (+ 1 day) [(get fish-count 1)
                        (get fish-count 2)
                        (get fish-count 3)
                        (get fish-count 4)
                        (get fish-count 5)
                        (get fish-count 6)
                        (+ (get fish-count 7) (get fish-count 0))
                        (get fish-count 8)
                        (get fish-count 0)]))))


(defn mode
  [vs]
  (let [fs (frequencies vs)]
    (first (last (sort-by second fs)))))


(defn quantile
  ([p vs]
   (let [svs (sort vs)]
     (quantile p (count vs) svs (first svs) (last svs))))
  ([p c svs mn mx]
   (let [pic (* p (inc c))
         k (int pic)
         d (- pic k)
         ndk (if (zero? k) mn (nth svs (dec k)))]
     (cond
       (zero? k) mn
       (= c (dec k)) mx
       (= c k) mx
       :else (+ ndk (* d (- (nth svs k) ndk)))))))


(defn median
  ([vs] (quantile 0.5 vs))
  ([sz svs mn mx] (quantile 0.5 sz svs mn mx)))


(defn mean
  ([vs] (mean (reduce + vs) (count vs)))
  ([sm sz] (/ sm sz)))


(defn standard-deviation
  ([vs]
   (standard-deviation vs (count vs) (mean vs)))
  ([vs sz u]
   (Math/sqrt (/ (reduce + (map #(Math/pow (- % u) 2) vs))
                 sz))))


(defn median-absolute-deviation
  ([vs]
   (median-absolute-deviation vs (median vs)))
  ([vs m]
   (median (map #(Math/abs (- % m)) vs))))


(defn lower-adjacent-value
  ([vs]
   (let [q1 (quantile 0.25 vs)
         m (median vs)
         q3 (quantile 0.75 vs)]
     (lower-adjacent-value (sort vs) m (- q3 q1))))
  ([svs m qd]
   (let [l (- m qd)]
     (first (filter (partial < l) svs)))))


(defn upper-adjacent-value
  ([vs]
   (let [q1 (quantile 0.25 vs)
         m (median vs)
         q3 (quantile 0.75 vs)]
     (upper-adjacent-value (reverse (sort vs)) m (- q3 q1))))
  ([rsvs m qd]
   (let [l (+ m qd)]
     (first (filter #(< % l) rsvs)))))


(defn stats-map
  ([vs]
   (let [sz (count vs)
         svs (sort vs)
         rsvs (reverse svs)
         mn (first svs)
         mx (first rsvs)
         sm (reduce + vs)
         u (mean sm sz)
         mdn (median sz svs mn mx)
         q1 (quantile 0.25 sz svs mn mx)
         q3 (quantile 0.75 sz svs mn mx)
         sd (standard-deviation vs sz u)
         mad (median-absolute-deviation vs mdn)
         qd (- q3 q1)
         lav (lower-adjacent-value svs mdn qd)
         uav (upper-adjacent-value rsvs mdn qd)]
     {:Size sz
      :Min mn
      :Max mx
      :Mean u
      :Median mdn
      :Mode (mode vs)
      :Q1 q1
      :Q3 q3
      :Total sm
      :SD sd
      :MAD mad
      :LAV lav
      :UAV uav}))
  ([ks vs]
   (zipmap ks (map (stats-map vs) ks))))


(let [ks [:Size :Min :Max :Mean :Median :Mode :Q1 :Q3 :Total :SD :MAD :LAV :UAV]]
  (defn summarise
    ([vs] (summarise "" vs))
    ([label vs]
     (apply format
            (str (reduce #(.append %1 %2)
                         (StringBuilder.)
                         (interpose \tab
                                    ["%1$s::"
                                     "Size: %2$.3f"
                                     "Total: %10$.3f"
                                     "Mean: %5$.3f"
                                     "Mode: %7$.3f"
                                     "Min: %3$.3f"
                                     "LAV: %13$.3f"
                                     "Q1: %8$.3f"
                                     "Median: %6$.3f"
                                     "Q3: %9$.3f"
                                     "UAV: %14$.3f"
                                     "Max: %4$.3f"
                                     "SD: %11$.3f"
                                     "MAD: %12$.3f"])))
            (conj (map (comp double (stats-map vs)) ks) label)))))


(defn closest-mean-fn
  [means]
  (fn [v] (reduce (partial min-key #(Math/pow (- v %) 2)) means)))


(defn k-means
  [k vs]
  (let [vs (map double vs)
        svs (set vs)]
    (if (> k (count svs))
      (sort svs)
      (loop [mns (sort (take k (shuffle svs)))
             pmns (repeat k Double/NaN)]
        (if (= mns pmns)
          mns
          (recur (sort (map mean (vals (group-by (closest-mean-fn mns) vs)))) mns))))))


;; Day 7
;; Task 1
(let [lines (str/split-lines (slurp "development/src/dev/jan/advent_of_code/input7.txt"))
      parsed (map edn/read-string (str/split (first lines) #","))
      subs (map (fn [[days fish]] [days (count fish)]) (group-by identity (map edn/read-string (str/split (first lines) #","))))
      ;; hvor mange over?
      ;; hvor mye koster det over?
      ]
  ;; (int (mean parsed)) 460...
  (apply min (map
               (fn [position] (reduce + (map (fn [pos] (let [distance (Math/abs (- pos position))] (/ (* distance (+ distance 1)) 2))) parsed)))
               (range 0 (apply max parsed)))))
