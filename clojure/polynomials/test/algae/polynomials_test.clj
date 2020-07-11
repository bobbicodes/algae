(ns algae.polynomials-test
  (:require [clojure.test :refer [deftest testing is]]
            [algae.polynomials :refer [mult-poly sub-poly]]))

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

(deftest mult-poly-test
  (testing "Multiply binomials by polynomials"
    (is (= [1 9 23 15 0 0 0] (mult-poly [1 4 3 0] [1 5 0 0])))
    (is (= [3 0 1 12 0 4] (mult-poly [3 0 1] [1 0 0 4])))
    (is (= [1 9 23 15 0 0 0] (mult-poly [1 4 3 0] [1 5 0 0])))
    (is (= [1 7 14 8] (mult-poly [1 4] [1 3 2])))
    (is (= [2 0 12 5 0 30 0] (mult-poly [2 0 0 5] [1 0 6 0])))
    (is (= [1 6 3 6 2 0] (mult-poly [1 6 2 0] [1 0 1])))
    (is (= [2 17 11 24 0] (mult-poly [1 8] [2 1 3 0])))))
