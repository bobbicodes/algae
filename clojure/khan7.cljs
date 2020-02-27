; find hypotenuse given 2 legs, or leg given leg & hypotenuse.

(defn hyp [a b]
  (let [hypotenuse (+ (* a a) (* b b))]
    (if (int? (sqrt hypotenuse))
                  (sqrt hypotenuse)
                  (str "Sqrt " hypotenuse))))

(defn leg [b c]
  (let [leg (- (* c c) (* b b))]
    (if (int? (sqrt leg))
                  (sqrt leg)
                  (str "Sqrt " leg))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn factors [num]
  (loop [n num div 2 divs '()]
    (cond (<= n 1) divs
          (zero? (rem n div)) (recur (/ n div) div (cons div divs))
          :else (recur n (inc div) divs))))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn pow [x n]
  (Math/pow x n))
 
(defn pt-delta [x] (let [a x b 6 c 8] (- (* c c) (+ (* a a) (* b b)))))

(defn top-one [big1 x]
  (if (> x big1)
    x
    big1))

(defn bottom1 [min1 x]
  (if (< x min1)
    x
    min1))

(defn calc-delta [A x n]
  (/ (- (/ A
             (pow x (- n 1)))
          x)
       n))
 
(defn nth-root
  ([x n] (nth-root x n 0.5 1.0))
  ([x n guess-prev guess-current]
   (if (< (abs (- guess-prev guess-current)) 1e-6)
     guess-current
     (recur x n guess-current (+ guess-current (calc-delta x guess-current n))))))

(defn sqrt [n]
  (nth-root n 2))

(defn cbrt [n]
  (nth-root n 3))

(defn percent [n parts]
    (* (/ 100 parts) n))

(percent 11 10)

(defn a-tri [b h]
  (* 1/2 b h))

(a-tri 4 5)

(defn r-cir [d]
  (/ d 2))

(defn d-cir [r]
  (* 2 r))

(r-cir 6)
(d-cir 8)
(/ 13 2)

(def pi 3.14)

(defn a-cir [r]
  (* r r pi))

(a-cir 3) 

(defn r-cir-c [c]
  (/ c (* 2 pi)))

(a-cir (r-cir-c 37.68))
(a-cir 5)

(filter #(= 9/5
            (/ (- % 12)
               10)) 
  (range -100 100))

; y=2x+5
; (2, __)
(loop [x 2 y 0]
  (if (= (+ (* 2 x) 5)
         y)
      y
      (recur x (inc y))))    

; 4x + y = 10
; (__, -6)

(loop [x 0 y -6]
  (if (= (+ (* 4 x) y)
         10)
      x
      (recur (inc x) y)))

; 5x - 2y = 30
; (8, __)

(loop [x 8 y 0]
  (if (= (- (* 5 x) (* 2 y))
         30)
      y
      (recur x (inc y))))

; y = -2x + 4
; (__, -2)

(loop [x 0 y -2]
  (if (= y
         (+ (* -2 x) 4))
      x
      (recur (inc x) y)))

; 2x + 5y = -6

(loop [x 0 y -1.4]
  (if (= (+ (* 2 x)
            (* 5 y))
         -6)
      y
    (recur x (+ y 0.1))))

(= (+ (* 2 0)
      (* 5 -1.4))
   -6)

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))
(round2 1 (+ -1.4 0.1))

#_(loop [x 0 y -1.4]
  (if (= (+ (* 2 x)
            (* 5 y))
         -6)
      y
    (recur x (+ y 0.1))))

(+ 6 7 8
