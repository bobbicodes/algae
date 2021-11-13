; https://www.khanacademy.org/math/algebra/one-variable-linear-inequalities/modal/e/interpretting-solving-linear-inequalities

; 100 + 50w >= 18000
; 50w >= 17900
(- 18000 100)
(/ 17900 50)
; w >= 358

; 34 - 12x > 8
(- 8 34)
; -12x > -26
; x < 26/12
; x < 13/6

; 32 <= 4p + 5
(- 32 5)
; 27 <= 4p
(/ 27 4)

; 3r + 24 >= 100
(- 100 24)
; 3r >= 76
(/ 76 3)

; 53w + 13 < 56w + 16
; 13 < 3w + 16
(- 13 16)
; -3 < 3w
(/ -3 3)

; -23d + 81 <= -98d + 1
; -23d + 80 <= -98d
(+ -98 23)
; 80 <= -75d
(/ 80 -75)
(/ 75 5)

; -7p - 4 > 26p - 94
; -7p > 26p - 90
(- -7 26)
; -33p > -90
(/ 90 33)
(/  3)
; p < 30

; -31p + 79 > -59p + 81
; -31p > -59p + 2
(+ -31 59)
; 28p > 2
(/ 2 28)
; p > 1/14

; 5x - 4 >= 12
; 5x >= 16
; x >= 16/5
; 12x + 5 <= -4
; 12x <= -9

; -7x - 50 <= -1
; -7x <= 49
(+ 50 -1)
; x >= -7
(/ 49 -7)
; -6x + 70 > -2
(- -2 70)
; -6x > -72
(/ -72 -6)
; x < 12

; 12x + 7 < -11
(- -11 7)
; 12x < -18

(/ -18 12)
; 5x - 8 >= 40
; 5x >= 48

; 3x - 91 > -87
(+ 91 -87)
; 3x > 4
; x > 4/3
; x > 48/5
; 17x - 16 > 18
(+ 16 18)
; 17x > 

; 6 > 2k + 4

(#(> 6
     (+ 4
        (* 2 %)))
  2)

; 2x + 3 >= 7
; 2x >= 4
; x >= 2


(#(or
    (>= (+ 3
           (* 2 %))
        7)
    (> (+ 9
          (* 2 %))
        11))
  1.5)

; 22q + 73 > 52q + 63
; 73 > 30q + 63
; 10 > 30q

(filter
  #(> (+ 73 (* 22 %))
      (+ 63 (* 52 %)))
  (range -100 100))
(- 52 22)

; 1.5m + 4 >= 7
(filter
  #(>= (+ 4 (* 1.5 %))
       7)
  (range -100 100))

; 30b + 53 >= 18b - 83
(filter
  #(>= (+ 53 (* 30 %))
       (- 83 (* 18 %)))
  (range -100 100))
(- -83 53)
; 30b >= 18b - 136
(- 30 18)
; 12b >= -136
(/ -136 12)
(/ -34 3)

; 10 >= j + 5

(#(>= 10
      (+ 5 %))
  5)

; -2x - 7 >= 41

(filter
  #(>= (- (* -2 %) 7)
       41)
  (range -100 100))

; -2x >= 48
(/ 48 -2)
; x <= -24

; -6x + 14 < -28
; AND
; 3x + 28 <= 25

(filter
  #(and
     #_(< (+ 14 (* -6 %))
        -28)
     (<= (+ 28 (* 3 %))
         25))
  (range -100 100))

; -6x + 14 < -28

; -8x + 14 >= 60
; OR
; -4x + 50 < 58

(filter
  #(or
     (>= (+ 14 (* -8 %))
         60)
     (< (+ 50 (* -4 %))
         58))
  (range -100 100))

; 750 - 70p < 450
(- 450 750)
; -70p < -300
(/ -300 -70)

; 55c + 13 <= 75c + 39
; 55c <= 75c + 26
(- 55 75)
; -20c <= 26
(/ 26 -20)

; 5x - 4 >= 12
; AND
; 12x + 5 <= -4
(filter
  #(and
     (>= (- (* 5 %) 4) 12)
     #_(<= (+ (* 12 %) 5) -4))
  (range -100 100))

; 5m + 1 <= 4
; 5m <= 3

(<= (+ 1 (* 5 2)) 4)

; 4r + 6 > 17
; 4r > 11
(/ 11 4)

(filter #(or
    (>= (+ 1 (* -7 %)) 22)
    (>= (+ 41 (* -10 %)) 81))
  (range -100 100))

; 5 < b - 3
(< 5 (- 10 3))

; -32c + 12 <= -66c - 16
(#(<= (+ 12 (* -32 %))
       (- (* -66 %) 16))
  -0.9)
; -32c <= -66c - 28
(+ 66 -32)
; 34c <= -28
(/ -14 17)

; -42v + 33 < 8v + 91
(- 91 33)
; -42v < 8v + 58
(- -42 8)
; -50v < 58
(/ 29 -25)
; v > -29/25

; 40n + 20 >= 260
; 40n >= 240
; n >= 6

; 12t - 2 < -5t + 36
; 12t < -5t + 38
; 17t < 38
; t < 38/17

(#(and
  (<= (+ 60 (* -15 %)) 105)
  (<= (+ 11 (* 14 %)) -31))   
-3)

; 5 >= 2p + 1
(>= 5 (+ 1 (* 2 2)))

; 4x + 4 <= 9x + 8
; 4x <= 9x + 4
(- 4 9)
; -5x <= 4
; x >= -4/5

(filter
  #(or
     (< (+ 4 (* 11 %)) 15)
     (> (- (* 12 %) 7) -25))
 (range -100 100))

(>= 7 (- 12 6))

; 13x - 8 <= -32
; 13x <= -24
(+ -32 8)
(< 7 (+ 3 10/2))

(filter
  #(and
     (>= (+ 39 (* 7 %)) 53)
     (> (+ 15 (* 16 %)) 31))
 (range -100 100))

(> 2 9/3)

; 64n - 6 >= 36n - 16
; 64n >= 36n - 10
(+ 6 -16)
(- 64 36)
; 28n >= -10
(/ -5 14)
; n >= -5/14
; 7l + 5 <= 60
; 7l <= 55
(/ 55 7)

(filter
  #(or
     (< (+ 60 (* -4 %)) 72)
     (< (+ 11 (* 14 %)) -31))
  (range -100 100))

(>= 2 (- 25/5 1))

; -65y + 19 < -2y + 41
; -65y < -2y + 22 
(- 41 19)
; -63y < 22
(/ 22 -63)

; 8m + 95 < -87m + 5
; 8m + 90 < -87m
(- -87 8)
; 90 < -95m
(/ 90 -95)
(/ 95 5)
; -18/19 > m
(/ 27 4)
(> 25/5 4)

(filter
  #(or
    (<= (+ (* -9 %) 5) 17)
    #_(<= (+ 25 (* 13 %)) -1))
 (range -100 100))

; 4t - 3 > 37t - 50
; 4t > 37t - 47
(- 4 37)
; -33t > -47
(/ 47 33)
; 4t - 3 > 37t - 50
(+ -3 50)
; 4t - 47 > 37t
(- 37 4)
; -47 > 33t
; (/ -47 33) > t
; -33t > -47
(/ -47 -33)

(<= (+ 5 15/3) 81) 

; 1.5m + 4 >= 7
(filter
  #(>= (+ 4 (* 1.5 %))
       7)
  (range -100 100))

(< (+ 7 5) 11)

(< (- (* 3 12) 7) 26)

; 34 - 12x

(filter
  #(and
    (<= (+ (* -9 %) 5) 17
    #_(<= (+ 25 (* 13 %))
 (range -100 100))

; 12x + 7 < -11
(- -11 7)
; 12x < -18

(/ -18 12)
; 5x - 8 >= 40
; 5x >= 48