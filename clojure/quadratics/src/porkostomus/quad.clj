(ns porkostomus.quad)

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

(defn simplify-sqrt [sqrt]
  (let [sq (reduce * (perfect-squares (prime-factors sqrt)))]
    [sq (/ sqrt (* sq sq))]))

(defn quadratic-rational [[a b c]]
  (let [discriminant (simplify-sqrt (- (* b b) (* 4 a c)))]
    [(/ (- b) (first discriminant))
     (last discriminant) (/ (* 2 a) (first discriminant))]))

(comment
  (quadratic-rational [3 24 48]))

(defn quadratic-roots [[a b c]]
  (let [m (* -1/2 (/ b a))
        d (Math/sqrt (- (* m m) (/ c a)))]
    [(+ m d) (- m d)]))

(defn graph [[a b c]]
  (let [vert-x (/ (- b) (* 2 a))
        vert-y (+ (* a (* vert-x vert-x))
                  (* b vert-x)
                  c)
        y-int [0 c]]
    {:vertex [vert-x vert-y]
     :y-int y-int}))

(comment
  (graph [-1 14 0])
(quadratic-roots [1 6 7]))