(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn exp [x n]
  (Math/pow x n))
 
(defn calc-delta [A x n]
  (/ (- (/ A (exp x (- n 1))) x) n))
 
(defn nth-root
  ([x n] (nth-root x n 0.5 1.0))
  ([x n guess-prev guess-current]
   (if (< (abs (- guess-prev guess-current)) 1e-6)
     guess-current
     (recur x n guess-current (+ guess-current (calc-delta x guess-current n))))))

(exp 2 -70)

(exp
  (* (exp 5 3) (exp 5 2))
  4)

(exp (exp 25 5) 4)
(exp 25 20)
(exp 8 0)
(reduce * (repeat 0 8))
(repeat 0 8)
(reduce * '())
(exp 7 1)

(defn cbrt [n]
  (nth-root n 3))

(defn sqrt [n]
  (nth-root n 2))

(cbrt 729)

(sqrt 1.96)
(sqrt 25)
(sqrt 196)
(sqrt 0.64)
(nth-root 32 5)
(+ 1 4/10)

(defn myfn [a b]
  (exp
    (/ (exp a -3)
       (exp b 2))
    4))

(defn fn-a [a b]
  (/ (exp b 2)
     (exp a 7)))

(defn fn-b [a b]
  (exp (/ b a) 20))

(defn fn-c [a b]
  (/ 1 (* (exp a 12)
          (exp b 8))))

(= (myfn 1 -1)
   (fn-c 1 -1))

(exp (* (exp 3 -8)
        (exp 7 3))
  -2)

(exp 21 10)

(* (exp 3 16)
   (exp 7 -6))

(def a -1)
(def b 68)

(=
  (exp (/ (exp a -3)
          (exp b 2))
    4)

  (/ 1 (* (exp a 12)
          (exp b 8))))

(exp (* (exp 3 -8)
        (exp 7 3))
  -2)

(/ (exp 7 6)
   (exp 3 16))

(exp (/ (exp 2 -10)
        (exp 4 2))
  7)

(exp 2 84)

(* (exp 2 -3)
   (exp 4 -9))

(/ 1 (* (exp 2 70)
        (exp 4 14)))

(def b 2)

  
  (exp (* (exp 5 4)
          (exp b -10))
    -6)

  (/ (exp b 60)
     (exp 5 24))

  #_(* (exp 5 24)
   (exp b 60))
  #_(* (exp 5 4)
     (exp b 60))

(def x 2)

(exp (/ (exp x 4)
        (exp 7 -8))
  -7)

(* (exp x -28)
   (exp 7 -56))

(exp (* (exp 3 3)
        (exp 6 6))
  -3)

(/ (exp 3 9)
   (exp 6 18))
(/ 1 (* (exp 3 9)
        (exp 6 18)))

(exp (/ (exp 3 -6)
        (exp 7 -3))
  5)

(/ (exp 3 15)
   (exp 7 30))

(/ (exp 7 15)
   (exp 3 30))

(def a 1)

(exp (* (exp a -2)
        (exp 8 7))
  2)

(exp 8 9)

(exp (* 8 a) 10)

(def y 3)

(exp (/ (exp 3 6)
        (exp y -5))
  2)

#_(/ (exp 3 12)
   (exp y -5))

#_(* (exp 3 12) (exp y 10))

(/ (exp 3 6)
   (exp y -10))

(def a 9)

(* (sqrt a)
   (- (* 2 (exp a 2))
      (/ 4 a)))

(let [z 1]
  (sqrt (exp (* 72 z) 5)))

(exp (* (exp 9 6)
        (exp 7 -9))
  -4)

(* (exp 9 24)
   (exp 7 -36))

(/ (exp 7 36)
   (exp 9 24))

(exp (/ (exp 4 3)
        (exp 5 -2))
  5)

(* (exp 4 15)
   (exp 5 10))

(/ (exp 4 15)
   (exp 5 10))

(/ (exp 4 8)
   (exp 5 3))

(def a 1)
(def b 1)

(exp (* (exp a -7)
        (exp b -2))
  -9)

(/ (* a 63)
   (* b 18))

(/ (* b 18)
   (* a 63))

(* (exp a 63)
   (exp b 18))

(exp (/ (exp b 7)
        (exp 4 5))
  -3)

(* (exp b -21)
   (exp 4 -15))

(/ (exp b 21)
   (exp 4 15))

(exp (* (exp 4 -2)
        (exp 4 -3))
  3)

(exp (exp 4 -5) 3)

(* 7 7 7 7 7)
(/ (exp 7 8)
   (exp 7 3))
(* (exp 7 5)
   (exp 7 1))

(* (/ 1 5)
   (/ 1 5)
   (/ 1 5)
   (/ 1 5))

(exp (exp 5 -2) 2)
(exp (exp 5 -4) 0)

(/ (exp 7 -3)
   (exp 7 -1))

(exp 7 -2)

(/ 1 (exp 7 2))

(* (exp 4 -2)
   (exp 7 -2))

(exp (* 4 7) -4)

(/ 1 (exp 28 2))

(exp (* (exp 5 3)
        (exp 5 2))
  4)

(exp 25 20)
(exp (exp 25 5) 4)

(+ -2 -3)
(* 7 7 7 7 7)
(/ (exp 7 8)
   (exp 7 3))
(* (exp 7 5)
   (exp 7 1))

(* (/ 1 5)
   (/ 1 5)
   (/ 1 5)
   (/ 1 5))

(exp (exp 5 -2) 2)
(exp (exp 5 -4) 0)

(/ (exp 7 -3)
   (exp 7 -1))

(exp 7 -2)
(/ 1 (exp 7 2))

(* (exp 4 -2)
   (exp 7 -2))

(exp (* 4 7) -4)

(exp (* (exp 5 3)
        (exp 5 2))
  4)
(exp 25 20)
(exp (exp 25 5) 4)

(cbrt 125)

(sqrt (/ 36 169))
(sqrt 1.69)
