(ns complex
  (:refer-clojure :rename {+ core+
                           - core-
                           / core-div
                           * core*}))

(defn complex [r i]
  [r i])

(defn complex? [x]
  (= (count x) 2))

(defn *
  ([v] v)
  ([[a1 b1] [a2 b2]]
   [(core- (core* a1 a2) (core* b1 b2)) (core+ (core* a1 b2) (core* a2 b1))]))

(defn + [v1 v2]
  (mapv core+ v1 v2))

(defn - [v1 v2]
  (mapv core- v1 v2))

(defn scale [s v]
  (mapv (partial core* s) v))

(defn divide-by-scalar [s v]
  (mapv #(core-div % s) v))

(defn / [v1 [a2 b2]]
  (divide-by-scalar (core+ (core* a2 a2) (core* b2 b2)) (* v1 [a2 (core- b2)])))

(comment
  (+ [1M -3M] [2M 5M])                   ;=> (3M 2M)
  (* [1M 1M] [1M 1M])                    ;=> [0M 2M]
  (scale 5M [1M 3M])                     ;=> (5M 15M)
  (/ [3M 3M] [3M 3M])                    ;=> (1M 0M)
  )