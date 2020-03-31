(ns algae.polynomials-test
  (:require [clojure.test :refer [deftest testing is]]
            [algae.polynomials :refer [print-poly sub-poly]]))

(deftest add-and-subtract-test
  (testing "Subtract polynomials."
    (is (= (sub-poly [-9 0 0 0 8] [-9 2 5 0 0])
           '(0 -2 -5 0 8)))
    (is (= (sub-poly [6 2 5] [5 -6 -11])
           '(1 8 16) ))
    (is (= (sub-poly [-7 3 -6] [3 4 4])
           '(-10 -1 -10)))
    (is (= (sub-poly [1 8 -9] [11 -4 7])
           '(-10 12 -16)))))
