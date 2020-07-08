(ns porkostomus.quad-test
  (:require [clojure.test :refer :all]
            [porkostomus.quad :refer :all]))

(defn round [d]
  (double (/ (Math/round (* d 100)) 100)))

(deftest quadratic-roots-test
    (is (= [-0.2 -1.0] (mapv round (quadratic-roots [5 6 1])))))
