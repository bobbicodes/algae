(ns algae.polynomials-test
  (:require [clojure.test :refer :all]
            [algae.polynomials :refer :all]))

(deftest a-test
  (testing "Subtract polynomial."
    (is (= (print-poly (sub-poly [-9 0 0 0 8] [-9 2 5 0 0]))
           " -2x^3 -5x^2 8 "))
    (is (= (print-poly (sub-poly [6 2 5] [5 -6 -11]))
           "1x^2 8x 16 " ))))
