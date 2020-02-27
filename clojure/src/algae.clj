(ns algae
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]))

(def equation
  (insta/parser
   "<equation> = left <'='> right
   left = expr+
   right = expr+
   <expr> = term | (expr '+' term) | (expr '-' term)
   <term> = factor | (term factor)
   <factor> = ('-'? number) | variable | ratio | parens
   parens = <'('> expr <')'>
   number = (#'[0-9]+' #'[.]' #'[0-9]+') | #'[0-9]+'
   variable = #'[A-Za-z]+'
   ratio = (number | variable) '/' '-'? (number | variable)"))

(defn left [eq] (vec (rest (first (equation eq)))))
(defn right [eq] (vec (rest (last (equation eq)))))

(defn parse-num [s]
  (cond
    (clojure.string/includes? s "/")
    (clojure.edn/read-string s)
    (clojure.string/includes? s ".")
    (Float/parseFloat s)
    :else (Integer/parseInt s)))

(defn expr-ast [expr]
  (match expr
         ["-" [:number n]] (- (parse-num n))
         [[:number n1] "+" [:number n2]] [(parse-num n1) (parse-num n2)]
         [[:number n1] "-" [:number n2]] [(parse-num n1) (- (parse-num n2))]
         [[:number n1] [:parens [:variable x] "+" [:number n2]]]
         {:multiplier (parse-num n1)
          :expr [{:variable x :multiplier 1} (parse-num n2)]}
         [[:number n1] [:parens [:variable x] "-" [:number n2]]]
         {:multiplier (parse-num n1)
          :expr [{:variable x :multiplier 1} (- (parse-num n2))]}
         ["-" [:number n1] [:parens [:variable x] "-" [:number n2]]]
         {:multiplier (- (parse-num n1))
          :expr [{:variable x :multiplier 1} (- (parse-num n2))]}
         [[:ratio [:variable x] "/" [:number n1]] "+" [:number n2]]
         [{:numer x :denom (parse-num n1)} (parse-num n2)]
         [[:ratio [:variable x] "/" [:number n1]] "-" [:number n2]]
         [{:numer x :denom (parse-num n1)} (- (parse-num n2))]
         [[:ratio [:variable x] "/" "-" [:number n1]] "+" [:number n2]]
         [{:numer x :denom (- (parse-num n1))} (parse-num n2)]
         [[:ratio [:number n1] "/" [:number n2]] "+" [:variable x]]
         [(parse-num (str n1 "/" n2)) {:variable x :multiplier 1}]
         [[:ratio [:number n1] "/" [:number n2]] [:variable x]]
         {:variable x :multiplier (parse-num (str n1 "/" n2))}
         [[:ratio [:variable x] "/" [:number d1 "." d2]]]
         {:numer x :denom (parse-num (str d1 "." d2))}
         [[:number n] "+" [:variable x]]
         [(parse-num n) {:variable x :multiplier 1}]
         [[:variable x] "+" [:number n]]
         [{:variable x :multiplier 1} (parse-num n)]
         [[:variable x] "+" [:number d1 "." d2]]
         [{:variable x :multiplier 1} (parse-num (str d1 "." d2))]
         [[:number d1 "." d2] "+" [:variable x]]
         [(parse-num (str d1 "." d2)) {:variable x :multiplier 1}]
         [[:number d1 "." d2] [:variable x]]
         {:variable x :multiplier (parse-num (str d1 "." d2))}
         [[:variable x] "-" [:number d1 "." d2]]
         [{:variable x :multiplier 1} (- (parse-num (str d1 "." d2)))]
         [[:variable x] "-" [:number n]]
         [{:variable x :multiplier 1} (- (parse-num n))]
         [[:variable x] "+" [:ratio [:number n1] "/" [:number n2]]]
         [{:variable x :multiplier 1} (parse-num (str n1 "/" n2))]
         [[:variable x] "-" [:ratio [:number n1] "/" [:number n2]]]
         [{:variable x :multiplier 1} (- (parse-num (str n1 "/" n2)))]
         [[:number n] "-" [:variable x]]
         [(parse-num n) {:variable x :multiplier -1}]
         [[:number n1] [:variable x] "+" [:number n2]]
         [{:multiplier (parse-num n1) :variable x} (parse-num n2)]
         ["-" [:number n1] [:variable x] "+" [:number n2]]
         [{:multiplier (- (parse-num n1)) :variable x} (parse-num n2)]
         [[:number n1] [:variable x] "-" [:number n2]]
         [{:multiplier (parse-num n1) :variable x} (- (parse-num n2))]
         [[:number n] [:variable x]]
         {:variable x :multiplier (parse-num n)}
         [[:number d1 "." d2]] (parse-num (str d1 "." d2))
         [[:number n1]] (parse-num n1)
         [[:ratio [:variable x] "/" [:number n]]]
         {:numer x :denom (parse-num n)}
         [[:ratio [:number n1] "/" [:number n2]]]
         (parse-num (str n1 "/" n2))
         :else "unrecognized pattern"))

(defn eq-ast [eq] {:left (expr-ast (left eq)) :right (expr-ast (right eq))})

(defn round [n]
  (if (float? n) (float (/ (Math/round (* n 100)) 100)) n))
(defn number [expr]
  (if (number? (first expr)) (first expr) (recur (rest expr))))
(defn variable [expr]
  (if (map? (first expr)) (:variable (first expr)) (recur (rest expr))))
(defn multiplier [expr]
  (if (map? (first expr)) (:multiplier (first expr)) (recur (rest expr))))
(defn numer [expr]
  (if (map? (first expr)) (:numer (first expr)) (recur (rest expr))))
(defn denom [expr]
  (if (map? (first expr)) (:denom (first expr)) (recur (rest expr))))

(defn solve [s]
  (let [l (:left (eq-ast s)) r (:right (eq-ast s))]
    (cond
      (and (number? r) (vector? l))
      (cond
        (variable l)
        (str (variable l) "=" (/ (- r (number l)) (multiplier l)))
        (numer l)
        (str (numer l) "=" (* (- r (number l)) (denom l))))
      (and (number? l) (vector? r))
      (cond
        (variable r)
        (str (variable r) "=" (/ (- l (number r)) (multiplier r)))
        (numer r)
        (str (numer r) "=" (* (- l (number r)) (denom r))))
      (and (number? r) (:expr l))
      (str (variable (:expr l)) "=" (- (/ r (:multiplier l)) (number (:expr l))))
      (and (number? l) (:expr r))
      (str (variable (:expr r)) "=" (- (/ l (:multiplier r)) (number (:expr r))))
      (and (number? l) (:variable r))
      (str (:variable r) "=" (/ l (:multiplier r)))
      (and (number? r) (:variable l))
      (str (:variable l) "=" (/ r (:multiplier l)))
      (and (number? r) (number? (:denom l)))
      (str (:numer l) "=" (round (* r (:denom l))))
      (and (number? l) (number? (:denom r)))
      (str (:numer r) "=" (round (* l (:denom r))))
      (number? r) (str (variable l) "=" (round (- r (number l))))
      (number? l) (str (variable r) "=" (round (- l (number r))))
      :else (eq-ast s))))
