(ns advent-of-code-2021-clojure.day14-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2021-clojure.day14 :refer :all]))

(deftest task1-test
  (is (= 17 (task1 (slurp "resources/input14_1.txt"))))
  )
(run-tests)
