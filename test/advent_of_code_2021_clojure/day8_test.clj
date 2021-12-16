(ns advent-of-code-2021-clojure.day8-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2021-clojure.day8 :refer :all]))

(deftest task1-test
  (is (= 1233 (task1 (slurp "resources/input1.txt"))))
  )
(run-tests)