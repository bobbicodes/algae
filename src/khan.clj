(ns khan)

(defn slope [x1 y1 x2 y2]
  (cond (= x1 x2)
              "Undefined"
              (= y1 y2)
              0
               :else
               (/ (- y2 y1)
                   (- x2 x1))))

(defn y-int [x1 y1 x2 y2]
  (- y1 (* (slope x1 y1 x2 y2) x1)))

(defn s-int [x1 y1 x2 y2]
  ((juxt slope y-int) x1 y1 x2 y2))

(defn eq [a b c d] (filter #(= (+ (* a %) b) (+ (* c %) d)) (range -200 200)))

(defn mid [a b c d] (+ (+ (* a (first (eq a b c d))) b) (+ (* c (first (eq a b c d))) d)))

(comment
  (mid 8 -5 5 4)
)