(ns algae.equations
  (:require [clojure.set :as set]))

(def e {:l {:c 12
            :n -4}
        :r {:c 14
            :n -10}})

(defn variables [side e]
  (filter
   #(not= :n %)
   (keys (side e))))

(defn subtract-var-r [e]
  (let [v (first (clojure.set/intersection
                  (set (variables :l e))
                  (set (variables :r e))))
        l-val (v (:l e))
        r-val (v (:r e))]
    {:l (assoc (:l e) v
               (- l-val r-val))
     :r (dissoc (:r e) v)}))

(defn subtract-l [e x]
  {:l (dissoc (:l e) x)
   :r (assoc (:r e)
             x (- (x (:r e))
                  (x (:l e))))})

(subtract-l (subtract-var-r e) :n)

(defn divide-l [e x]
  {:l {x 1}
   :r (zipmap
       (keys (:r e))
       (map #(/ %
                (x (:l e)))
            (vals (:r e))))})

(divide-l (subtract-l (subtract-var-r e) :n) :c)

(defn subtract-l [e x]
  {:l (dissoc (:l e) x)
   :r (assoc (:r e)
             x (- (x (:r e))
                  (x (:l e))))})

(defn mult-frac-l [e x]
  (let [denominator (last (:frac (:l e)))]
    {:l {x 1}
     :r {:n (* (:n (:r e)) denominator)}}))

(mult-frac-l
 (subtract-l
  {:l {:frac [:j -2]
       :n 7}
   :r {:n -12}} :n) :j)

(defn subtract-r [e x]
  {:l (assoc (:l e) x
             (- (x (:l e))
                (x (:r e))))
   :r (dissoc (:r e) x)})

(defn div-exp-r [e x]
  {:l {:n (/ (:n (:l e))
             (:mult (:exp (:r e))))}
   :r (:paren (:exp (:r e)))})

(subtract-r
 (div-exp-r
  {:l {:n 24}
   :r {:exp {:mult 3
             :paren {:x 1 :n -5}}}}
  :x) :n)

(defn multiply-r [e x]
  {:l (zipmap
       (keys (:l e))
       (map #(* %
                (x (:r e)))
            (vals (:l e))))
   :r {x 1}})

(defn multiply-l [e x]
  {:l {x 1}
   :r (zipmap
       (keys (:r e))
       (map #(* %
                (x (:l e)))
            (vals (:r e))))})

(defn divide-l [e x]
  {:l {x 1}
   :r (zipmap
       (keys (:r e))
       (map #(/ %
                (x (:l e)))
            (vals (:r e))))})

(defn divide-r [e x]
  {:l (zipmap
       (keys (:l e))
       (map #(/ %
                (x (:r e)))
            (vals (:l e))))
   :r {x 1}})

(defn subtract-l [e x]
  {:l (dissoc (:l e) x)
   :r (assoc (:r e)
             x (- (x (:r e))
                  (x (:l e))))})

(defn subtract-r [e x]
  {:l (assoc (:l e) x
             (- (x (:l e))
                (x (:r e))))
   :r (dissoc (:r e) x)})

(defn mult-frac-l [e x]
  (let [denominator (last (:frac (:l e)))]
    {:l {x 1}
     :r {:n (* (:n (:r e)) denominator)}}))

(defn div-exp-r [e x]
  {:l {:n (/ (:n (:l e))
             (:mult (:exp (:r e))))}
   :r (:paren (:exp (:r e)))})

(defn mult-frac-r [e x]
  (let [denominator (last (:frac (:r e)))]
    {:r {:n (* (:n (:l e)) denominator)}
     :l {x 1}}))

(mult-frac-r (subtract-r {:l {:n 2}
                          :r {:frac [:m 2]
                              :n -7}} :n) :m)

