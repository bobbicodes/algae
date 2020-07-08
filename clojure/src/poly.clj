(ns poly)

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

(defn mult-binomial-poly [[p1 p2 p3][b1 b2]]
  [(* b1 p1)
   (+ (* b2 p1) (* b1 p2))
   (+ (* b2 p2) (* b1 p3))
   (* b2 p3)])

(comment
  (mult-binomial-poly polynomial binomial)
  )