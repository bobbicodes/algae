(ns complex)

; Express the radical using the imaginary unit, i.
; Express your answer in simplified form.

; ±sqrt{-36}=±

(defn prime-factors
  ([n] (prime-factors 2 n))
  ([f n]
   (when (> n 1)
     (if (zero? (mod n f))
       (cons f (prime-factors f (/ n f)))
       (recur (inc f) n)))))

(defn perfect-squares [s]
  (loop [items (sort s) pairs []]
    (if (empty? items) pairs
        (if (= (first items) (second items))
          (recur (drop 2 items) (conj pairs (first items)))
          (recur (rest items) pairs)))))

(defn simplify-negative-root [sqrt]
  (reduce * (perfect-squares (prime-factors (Math/abs sqrt)))))
  
(simplify-negative-root -36)

(defn complex [r i]
  [r i])

(defn complex? [x]
  (= (count x) 2))

(defn mult
  ([v] v)
  ([[a1 b1] [a2 b2]]
   [(- (* a1 a2) (* b1 b2)) (+ (* a1 b2) (* a2 b1))]))

(defn add [v1 v2]
  (mapv + v1 v2))

(defn sub [v1 v2]
  (mapv - v1 v2))

(defn scale [s v]
  (mapv (partial * s) v))

(defn divide-by-scalar [s v]
  (mapv #(/ % s) v))

(defn div [v1 [a2 b2]]
  (divide-by-scalar (+ (* a2 a2) (* b2 b2)) (* v1 [a2 (- b2)])))

(comment
  (add [1M -3M] [2M 5M])                   ;=> (3M 2M)
  (mult [1M 1M] [1M 1M])                    ;=> [0M 2M]
  (scale 5M [1M 3M])                     ;=> (5M 15M)
  (div [3M 3M] [3M 3M])                    ;=> (1M 0M)
  )