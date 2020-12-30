(ns algae.polynomials)

(defrecord Rational [^long numerator ^long denominator])

(defmethod print-method Rational [v ^java.io.Writer w]
  (print-method (:numerator v) w)
  (.write w "/")
  (print-method (:denominator v) w))

(defrecord Complex [^long real ^long imaginary])

(defmethod print-method Complex [v ^java.io.Writer w]
  (print-method (:real v) w)
  (.write w "+")
  (print-method (:imaginary v) w)
  (.write w "i"))

(defrecord Poly [variable term-list])

(defmethod print-method Poly [v ^java.io.Writer w]
  (print-method (:variable v) w)
  (.write w ":")
  (print-method (:term-list v) w))

(defprotocol Algebra
  (add [a b])
  (sub [a b])
  (mul [a b])
  (div [a b])
  (equal? [a b]))

(defn round [n]
  (if (>= n 0.0)
    (Math/floor n)
    (Math/ceil n)))

(defn square [x]
  (mul x x))

; Complex numbers

(defn real-part [rect]
  (get rect :real))

(defn imag-part [rect]
  (get rect :imaginary))

(defn magnitude [polar]
  (Math/sqrt (add (square (real-part polar))
                  (square (imag-part polar)))))

(defn angle [polar]
  (Math/atan2 (imag-part polar)
              (real-part polar)))

(defn make-from-real-imag [x y]
  (Complex. x y))

(defn make-from-mag-ang [r a]
  (Complex. (int (round (mul r (Math/cos a))))
            (int (round (mul r (Math/sin a))))))

; Polynomials

(defn variable [p]
  (get p :variable))

(defn term-list [p]
  (get p :term-list))

(defn order [term]
  (first term))

(defn coeff [term]
  (fnext term))

(defn make-term [order coeff]
  (list order coeff))

(defn first-term [term-list]
  (first term-list))

(defn first-term-dense [term-list]
  (concat
   (first term-list)
   (- (count term-list) 1)))

(defn rest-terms [term-list]
  (next term-list))

(defn adjoin-term [term term-list]
  (cons term term-list))

(defn adjoin-term-dense [term term-list]
  (if (zero? (coeff term))
    term-list
    (cons (coeff term) term-list)))

(defn sparse-to-dense [p]
  (let [poly (reverse (term-list p))
        diff-terms (map #(vector (dec (- (first %1) (first %2))) (second %1))
                        (next poly) poly)]
    (->> diff-terms
         (cons (first poly))
         (mapcat #(concat (repeat (first %) 0) [(second %)]))
         (reverse)
         (#(Poly. (variable p) %)))))

(defn dense-to-sparse [p]
  (->> (term-list p)
       (reverse)
       (map-indexed #(if (not= %2 0) (list %1 %2)))
       (filter some?)
       (reverse)
       (#(Poly. (variable p) %))))

; Rationals

(defn numer [x]
  (get x :numerator))

(defn denom [x]
  (get x :denominator))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn extended-gcd [a b]
  (let [class-a (class a)]
    (cond
      (number? a)        (gcd a b)
      (= class-a Rational) (reduce gcd (list (numer a) (denom a) (numer b) (denom b)))
      (= class-a Complex)  (reduce gcd (list (real-part a) (imag-part a) (real-part b) (imag-part b)))
      (= class-a Poly)     (reduce gcd (concat (term-list a) (term-list b))))))

(defn make-rat [n d]
  (let [g (extended-gcd n d)]
    (Rational. (div n g) (div d g))))

; Type coercion

(defn raise-types [a b proc]
  (let [class-a (class a)
        class-b (class b)]
    (cond
      (and (number? a) (= class-b Rational))
      (proc (Rational. a 1) b)
      (and (number? a)          (= class-b Complex))   
      (proc (Complex. a 0) b)
      (and (number? a)          (= class-b Poly))     
      (proc (Poly. (variable b) (list (list 0 a))) b)
      (and (= class-a Rational) (number? b))          
      (proc a (Rational. b 1))
      (and (= class-a Rational) (= class-b Complex))  
      (proc (Complex. a 0) b)
      (and (= class-a Rational) (= class-b Poly))    
      (proc (Poly. (variable b) (list (list 0 a))) b)
      (and (= class-a Complex)  (number? b))         
      (proc a (Complex. b 0))
      (and (= class-a Complex)  (= class-b Rational))  
      (proc a (Complex. b 0))
      (and (= class-a Complex)  (= class-b Poly))      
      (proc (Poly. (variable b) (list (list 0 a))) b)
      (and (= class-a Poly)     (number? b))          
      (proc a (Poly. (variable a) (list (list 0 b))))
      (and (= class-a Poly)     (= class-b Rational)) 
      (proc a (Poly. (variable a) (list (list 0 b))))
      (and (= class-a Poly)     (= class-b Complex))  
      (proc a (Poly. (variable a) (list (list 0 b)))))))

(defn reduce-type [a]
  (let [class-a (class a)]
    (cond
      (and (= class-a Rational)
           (= (denom a) 1))          
      (numer a)
      (and (= class-a Complex)
           (= (imag-part a) 0))                
      (reduce-type (real-part a))
      (and (= class-a Poly)
           (empty (rest-terms (term-list a)))
           (= (order (first-term (term-list a))) 0)) 
      (reduce-type (coeff (first-term (term-list a))))
      :else a)))

(extend-type Number
  Algebra
  (add [a b]
    (if (= (class a) (class b))
      (+ a b)
      (raise-types a b add)))
  (sub [a b]
    (if (= (class a) (class b))
      (- a b)
      (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (* a b)
      (raise-types a b mul)))
  (div [a b]
    (if (= (class a) (class b))
      (/ a b)
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (= a b)
      (raise-types a b equal?))))

(extend-type Rational
  Algebra
  (add [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (add (mul (numer a) (denom b))
                      (mul (numer b) (denom a)))
                 (mul (denom a) (denom b))))
      (raise-types a b add)))
  (sub [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (sub (mul (numer a) (denom b))
                      (mul (numer b) (denom a)))
                 (mul (denom a) (denom b))))
      (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (mul (numer a) (numer b))
                 (mul (denom a) (denom b))))
      (raise-types a b mul)))
  (div [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (mul (numer a) (denom b))
                 (mul (denom a) (numer b))))
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (let [simple-a (make-rat (numer a) (denom a))
            simple-b (make-rat (numer b) (denom b))]
        (if (and (equal? (numer simple-a) (numer simple-b))
                 (equal? (denom simple-a) (denom simple-b)))
          true
          false))
      (raise-types a b equal?))))

(extend-type Complex
  Algebra
  (add [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-from-real-imag (add (real-part a) (real-part b))
                            (add (imag-part a) (imag-part b))))
      (raise-types a b add)))
  (sub [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-from-real-imag (sub (real-part a) (real-part b))
                            (sub (imag-part a) (imag-part b))))
      (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-from-mag-ang (mul (magnitude a) (magnitude b))
                          (add (angle a) (angle b))))
      (raise-types a b mul)))
  (div [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-from-mag-ang (div (magnitude a) (magnitude b))
                          (sub (angle a) (angle b))))
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (if (and (equal? (real-part a) (real-part b))
               (equal? (imag-part a) (imag-part b)))
        true
        false)
      (raise-types a b equal?))))

(defn negate-terms [termlist]
  (map
   (fn [t]
     (make-term (order t)
                (sub 0 (coeff t))))
   termlist))

(defn add-terms [l1 l2]
  (cond
    (and (empty? l1) (empty? l2)) '()
    (empty? l1) l2
    (empty? l2) l1
    :else
    (let [t1 (first-term l1)
          t2 (first-term l2)]
      (cond
        (> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms l1) l2))
        (< (order t1) (order t2)) (adjoin-term t2 (add-terms l1 (rest-terms l2)))
        :else
        (adjoin-term (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms l1)
                                (rest-terms l2)))))))

(defn mul-term-by-all-terms [t1 l]
  (if (empty? l)
    l
    (let [t2 (first-term l)]
      (adjoin-term
       (make-term (+ (order t1) (order t2))
                  (mul (coeff t1) (coeff t2)))
       (mul-term-by-all-terms t1 (rest-terms l))))))

(defn mul-terms [l1 l2]
  (if (empty? l1)
    l1
    (add-terms (mul-term-by-all-terms (first-term l1) l2)
               (mul-terms (rest-terms l1) l2))))

(defn div-terms [l1 l2]
  (if (empty? l1)
    l1
    (let [t1 (first-term l1)
          t2 (first-term l2)]
      (if (> (order t2) (order t1))
        l1
        (let [new-c (div (coeff t1) (coeff t2))
              new-o (- (order t1) (order t2))]
          (let [rest-of-result
                (div-terms
                 (add-terms l1
                            (negate-terms
                             (mul-terms l2
                                        (list
                                         (make-term new-o new-c)))))
                 l2)]
            (list (adjoin-term (make-term new-o new-c)
                               (first rest-of-result))
                  (fnext rest-of-result))))))))

(extend-type Poly
  Algebra
  (add [a b]
    (if (= (class a) (class b))
      (reduce-type
       (if (= (variable a) (variable b))
         (Poly. (variable a)
                (add-terms (term-list a)
                           (term-list b)))
         (println "ERROR: Polys not in same var -- ADD-POLY"
                  (list a b))))
      (raise-types a b add)))
  (sub [a b]
    (if (= (class a) (class b))
      (reduce-type
       (if (= (variable a) (variable b))
         (Poly. (variable a)
                (add-terms (term-list a)
                           (negate-terms (term-list b))))
         (println "ERROR: Polys not in same var -- SUB-POLY"
                  (list a b))))
      (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (reduce-type
       (if (= (variable a) (variable b))
         (Poly. (variable a)
                (mul-terms (term-list a)
                           (term-list b)))
         (println "ERROR: Polys not in same var -- MUL-POLY"
                  (list a b))))
      (raise-types a b mul)))
  (div [a b]
    (if (= (class a) (class b))
      (reduce-type
       (if (= (variable a) (variable b))
         (let [result (div-terms (term-list a)
                                 (term-list b))]
           (Poly. (variable a) (first result)))
         (println "ERROR: Polys not in same var -- DIV-POLY"
                  (list a b))))
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (reduce-type
       (let [sparse-a (dense-to-sparse a)
             sparse-b (dense-to-sparse b)]
         (if (= (variable sparse-a) (variable sparse-b))
           (not-any? false?
                     (map equal? (term-list sparse-a) (term-list sparse-b)))
           false)))
      (raise-types a b equal?))))

(defn mult-poly [poly1 poly2]
  (vec (:term-list (sparse-to-dense (mul (dense-to-sparse (Poly. 'b poly1)) (dense-to-sparse (Poly. 'b poly2)))))))

(comment
  
  (dense-to-sparse (Poly. 'k '(1 0 7)))
  
  (mult-poly [1 0 0 0 9 0 0] [-1 0 0 0 9 0 0]))

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

; https://www.khanacademy.org/math/algebra2/x2ec2f6f830c9fb89:poly-arithmetic/x2ec2f6f830c9fb89:bi-by-poly/e/bi-by-poly-area

; A rectangle has a height of t^2+7t+6 and a width of 2t+1.
; Express the area of the entire rectangle.
; Your answer should be a polynomial in standard form.

; The area of each smaller rectangle is the product of its height and its width.
; Once we find the areas of all the smaller rectangles, we can add them up to get an expression for the entire area.
; 
; Now let's add all the areas of the smaller rectangles and combine like terms.
; 2t^3 + t^2 + 14t^2 + 7t + 12t + 6
; = 2t^3 + 15t^2 + 19t + 6
 
; Represent t^2+7t+6

(def polynomial [1 7 6])

; Represent 2t+1

(def binomial [2 1])

; We need to take this and get [2 15 19 6]

(defn mult-bi-poly [[p1 p2 p3] [b1 b2]]
  [(* b1 p1)
   (+ (* b2 p1) (* b1 p2))
   (+ (* b2 p2) (* b1 p3))
   (* b2 p3)])

(comment
  (mult-bi-poly [1 7 6] [2 1])
  (mult-bi-poly [1 0 3] [1 7]))

; A rectangle has a height of k^2+3 and a width of k^2+7.
; Express the area of the entire rectangle.

(def height 
  (dense-to-sparse (Poly. 'k [1 0 3])))

(def width
  (dense-to-sparse (Poly. 'k [1 0 7])))

(comment 
  (mult-poly [1 0 3] [1 0 7])
  (mult-poly [1 3 9] [1 0 2])
  (mult-poly [1 0 3] [1 2 5])
  (mult-poly [1 9] [1 2 0]))

; Then the next one is:
; 
; A rectangle has a height of b^3+b^2
; and a width of b^2+7b+4
; Express the area of the entire rectangle.
; Your answer should be a polynomial in standard form.

; We represent it like this:

; [1 1 0 0] [0 1 7 4]
; 
; and it needs to get to this:
; 
; b^5+8b^4+11b^3+4b^2
; [1 8 11 4 0 0]

; this is done by first multiplying . . .

; . . . [return to this later?]

; Average rate of change (ARC) of polynomials
; https://www.khanacademy.org/math/algebra2/x2ec2f6f830c9fb89:poly-arithmetic/x2ec2f6f830c9fb89:poly-avg-rate/e/avg-rate-of-change?modal=1

; f(x)=x^2+10

; What is the average rate of change of f over the interval [-2,-1]?

(defn arc [t1 t2 p1 p2]
  (/ (- (+ (square p1) t2)
        (+ (square p2) t2))
     (- p1 p2)))

(comment
  (arc nil 10 -1 2)
  )

; g(x) = -(x^2/4) + 7

(defn g [x]
  (+ (/ (- (square x)) 4) 7))

; What is the average rate of change of g over the interval [-2,4]?

(defn arc [p1 p2]
  (/ (- (g p1)
        (g p2))
     (- p1 p2)))

(comment
  (g 4)
  (arc -2 4)
  
  )

; f(x) = x^2 − x − 1
; Over which interval does f have an average rate of change of zero?

(defn f [x]
  (- (- (square x) x)
     1))

(f 2)
(f 3)
(f -1)

; f(x) = x^3 - 9x
; What is the average rate of change of f over the interval [1,6] [1,6]?

(defn f [x]
  (- (* x x x)
     (* 9 x)))

(defn arc [p1 p2]
  (/ (- (f p1)
        (f p2))
     (- p1 p2)))

(arc 1 6)

; g(t) = −(t−1)^2 + 5
; Over which interval does g have an average rate of change of zero?

(defn g [t]
  (+ (- (square (- t 1))) 5))

; let's abstract the thing

(defn arc [f p1 p2]
  (/ (- (f p1)
        (f p2))
     (- p1 p2)))

(arc g -2 0)
(arc g -4 -3)
(arc g -2 4)

; f(x) =x^2 − x − 1
; What is the average rate of change of f over the interval -1<=x<=1?

(defn f [x]
  (- (- (square x) x) 1))

(arc f -1 1)
(arc f -3 -2)
(arc f 2 3)
(arc f -1 2)

; h (x) = x^2 − 1
; Over which interval does h have a negative average rate of change?

(defn h [x]
  (- (square x) 1))

(arc h -1 5)
(arc h -3 5)
(arc h -3 1)

; f(x)=x^2 +10
; What is the average rate of change of f over the interval [-2,-1]?

(defn f [x]
  (+ 10 (square x)))

(arc f -2 -1)

(defn arc-zero? [p1 p2]
  (= (+ (- (square (- p1 1)))
        5)
     (+ (- (square (- p2 1)))
        5)))

(comment
  (arc-zero? -2 0)
  (arc-zero? -2 4)
  )
