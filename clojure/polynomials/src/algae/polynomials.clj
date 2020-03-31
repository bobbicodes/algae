(ns algae.polynomials)

(defn zero-pad [poly1 poly2]
  (cond
    (> (count poly1) (count poly2))
    (recur poly1 (cons 0 poly2))
    (> (count poly2) (count poly1))
    (recur (cons 0 poly1) poly2)
    :else [poly1 poly2]))

(defn add-poly [poly1 poly2]
  (apply map + (zero-pad poly1 poly2)))

(defn sub-poly [poly1 poly2]
  (apply map + (zero-pad poly1 (map - poly2))))

(defn print-poly [poly]
  (loop [n (dec (count poly))
         p poly
         s ""]
    (if (empty? p)
      s
      (recur (dec n)
             (rest p)
             (if (= 0 (first p))
               (str s " ")
               (str s (first p) (if (= 0 n)
                                  " "
                                  (if (= 1 n)
                                    "x "
                                    (str "x^" n " ")))))))))
(comment
  (print-poly (sub-poly [6 2 5] [5 -6 -11]))
  (print-poly (sub-poly [-9 0 0 0 8] [-9 2 5 0 0]))
  (print-poly (sub-poly [-7 3 -6] [3 4 4]))
  (print-poly (sub-poly [1 8 -9] [11 -4 7])))