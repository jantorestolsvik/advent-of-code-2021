(ns advent-of-code-2021-clojure.day1-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2021-clojure.day1 :refer [task1]]))

(deftest task1-test
  (is (= 1233 (task1 (slurp "resources/input1.txt"))))
  )

(deftest task2-test
  (is (= 1275 (advent-of-code-2021-clojure.day1/task2 (slurp "resources/input1.txt"))))
  )
(run-tests)
