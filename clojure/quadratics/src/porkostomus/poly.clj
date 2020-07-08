(ns poly
  "Solve polynomials using Durand Kerner method"
  (:require complex))
;; helper functions to initialize algorithm

(defn initialize-xs
  [coefs & {:keys [base] :or {base [0.4M 0.9M]}}]
  (loop [vi [1M 0M]
         vs []]
    (if (>= (count vs) (dec (count coefs)))
      vs
      (recur (complex/* vi base) (conj vs vi)))))

;; Polynomial function
(defn eval-polynomial [coefs x]
  (loop [[coef & tail] (reverse coefs)
         x-pow         [1M 0M]
         result        [0M 0M]]
    (if coef
      (recur tail (complex/* x-pow x) (complex/+ result (complex/scale coef x-pow)))
      result)))

;; Durand-Kerner Method implementation
(defn- step [f xs i]
  (update xs i
          #(complex/- % (complex// (f %)
                                   (transduce (map-indexed (fn [j k] (if (= j i) [1M 0M] (complex/- % k))))
                                              complex/* [1M 0M] xs)))))

(defn find-roots
  [coefs & {:keys [max-iterations precision] :or {max-iterations 30
                                                  precision      5}}]
  (let [degree (dec (count coefs))
        f      (partial eval-polynomial (map #(/ % (first coefs)) coefs))]
    (loop [xs (initialize-xs coefs)
           i  0]
      (let [xs' (->> (range degree)
                     (reduce (partial step f) xs))]
        (if (or (with-precision precision (= xs xs'))
                (>= i max-iterations))
          xs'
          (recur xs' (inc i)))))))

(comment
  (initialize-xs [1M -3M])

  (eval-polynomial [3M 1M] [0M 1M])

  (with-precision 5 (find-roots [5M 6M 1M]))       ;=> [[-1.0000M 0E-20M] [-0.20000M 0E-23M]]
  (with-precision 5 (find-roots [1M -3M 3M -5M])) ; => [[2.5873M -1.2E-109M] [0.20630M 1.3747M] [0.20630M -1.3747M]]
  )