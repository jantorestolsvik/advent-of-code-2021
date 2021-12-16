(ns advent-of-code-2021-clojure.day2-test
  (:require [clojure.test :refer :all])
  (:require [advent-of-code-2021-clojure.day2 :refer :all]))

(deftest task1-test
  (is (= 1698735 (task1 (slurp "resources/input2.txt"))))
  )

(deftest task2-test
  (is (= 1594785890 (task2 (slurp "resources/input2.txt"))))
  )
(run-tests)
