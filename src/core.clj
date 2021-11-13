(ns algae.core
  (:require [instaparse.core :as insta]))

(def equation
  (insta/parser
    "equation = expression <whitespace>* comparator <whitespace>* expression
     expression = ((number | variable | operator) <whitespace>*)+ 
     whitespace = #'\\s+'
     comparator = '=' | '<' | '>'
     operator = '+' | '-' | '*' | '/'
     variable = number* #'[a-zA-Z]+'
     number = #'[0-9]+'"))

(comment

(equation "3x + 5 = 2 - y")

[:equation
  [:expression
    [:variable
      [:number "3"] "x"]
    [:operator "+"]
    [:number "5"]]
  [:comparator "="]
  [:expression
    [:number "2"]
    [:operator "-"]
    [:variable "y"]]]
)
; https://eddmann.com/posts/infix-calculator-in-clojure/
(defn tokenize [expr]
  (let [to-chars #(clojure.string/split (clojure.string/replace % " " "") #"")
        is-digit? #(and % (re-find #"^\d+$" %))]
    (reverse
      (reduce
        (fn [[t & ts :as tokens] token]
          (if (and (is-digit? token) (is-digit? t))
            (cons (str t token) ts)
            (cons token tokens)))
        '(), (to-chars expr)))))

; (tokenize "3 + 4 * 5 / (3 + 2)")

(defn shunting-yard [tokens]
  (let [ops {"+" 1, "-" 1, "*" 2, "/" 2}]
    (flatten
      (reduce
        (fn [[rpn stack] token]
          (let [less-op? #(and (contains? ops %) (<= (ops token) (ops %)))
                not-open-paren? #(not= "(" %)]
            (cond
              (= token "(") [rpn (cons token stack)]
              (= token ")") [(vec (concat rpn (take-while not-open-paren? stack))) (rest (drop-while not-open-paren? stack))]
              (contains? ops token) [(vec (concat rpn (take-while less-op? stack))) (cons token (drop-while less-op? stack))]
              :else [(conj rpn token) stack])))
        [[] ()]
        tokens))))

;(tokenize "3 + 4 * 5 / (3 + 2)")
;((comp shunting-yard tokenize) "3 + 4 * 5 / (3 + 2)")

(defn rpn [tokens]
  (let [ops {"+" +, "-" -, "*" *, "/" /}]
    (first
      (reduce
        (fn [stack token]
          (if (contains? ops token)
            (cons ((ops token) (second stack) (first stack)) (drop 2 stack))
            (cons (read-string token) stack)))
        [] tokens))))

; ((comp rpn shunting-yard tokenize) "3 + 4 * 5 / (3 + 2)")
