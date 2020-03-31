(ns algae.polynomials-test
  (:require [clojure.test :refer [deftest testing is]]
            [algae.polynomials :refer [print-poly sub-poly]]))

(deftest add-and-subtract-test
  (testing "Subtract polynomials."
    (is (= (print-poly (sub-poly [-9 0 0 0 8] [-9 2 5 0 0]))
           " -2x^3 -5x^2  8 "))
    (is (= (print-poly (sub-poly [6 2 5] [5 -6 -11]))
           "1x^2 8x 16 " ))
    (is (= (print-poly (sub-poly [-7 3 -6] [3 4 4]))
           "-10x^2 -1x -10 "))
    (is (= (print-poly (sub-poly [1 8 -9] [11 -4 7]))
           "-10x^2 12x -16 "))))
