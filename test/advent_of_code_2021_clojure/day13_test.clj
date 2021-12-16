(ns advent-of-code-2021-clojure.day13-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2021-clojure.day13 :refer :all]))

(deftest task1-test
  (is (= 17 (task1 (slurp "resources/input13_1.txt"))))
  (is (= 827 (task1 (slurp "resources/input13.txt"))))
  )

(task2 (slurp "resources/input13.txt"))

(run-tests)

