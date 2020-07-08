(ns poly-test
  (:require [clojure.test :refer :all]
            [poly :refer :all]))

(deftest mult-binomial-poly-test
  (is (= [2 15 19 6] (mult-binomial-poly [1 7 6] [2 1]))))
