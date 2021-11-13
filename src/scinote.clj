(defn scinote [n]
  (loop [n n  acc 0]
     (if (< 10 n)
          (recur (/ n 10) (inc acc))
           (str n " x 10^" acc))))

(defn scinote-less [n]
  (loop [n n  acc 0]
     (if (> 1 n)
          (recur (* n 10) (dec acc))
           (str n " x 10^" acc))))

(defn sci-add [s1 e1 s2 e2]
  (+ (* s1 (Math/pow 10 e1))
       (* s2 (Math/pow 10 e2))))

(defn sci-sub [s1 e1 s2 e2]
  (- (* s1 (Math/pow 10 e1))
      (* s2 (Math/pow 10 e2))))

(defn sci-mul [s1 e1 s2 e2]
  (scinote (* (* s1 (Math/pow 10 e1))
                       (* s2 (Math/pow 10 e2)))))

(defn sci-div [s1 e1 s2 e2]
  (/ (* s1 (Math/pow 10 e1))
      (* s2 (Math/pow 10 e2))))