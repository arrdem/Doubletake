(ns doubletake.parser
  (:use clojure.set
        name.choi.joshua.fnparse))

(def digit
  (alt
    (lit "1") (lit "2") (lit "3") (lit "4") (lit "5") (lit "6") (lit "7")
    (lit "8") (lit "9") (lit "0")))

(def literal
  ; "<literal> ::= <digit> | (<digit><literal>)"
  (alt
    (conc digit literal)
    digit))

(def operator
  ; "<operator> ::= +|-|*|/|%"
  (alt
    (lit "+")
    (lit "-")
    (lit "/")
    (lit "%")
    (lit "*")))

(def l-bracket (lit "["))
(def r-bracket (lit "]"))

(def expr-head
  ; "<expr head> ::= <operator><l bracket>"
  (conc
    operator
    l-bracket))

(def expr-tail
  ; "<expr tail> ::= <r bracket>"
  (conc
    r-bracket))

(def expression
  ; "<expression> ::= "
  (conc
    expr-head
    (rep+
      (conc
        (alt
          expression
          literal)
        (lit ";")))
    expr-tail
    )
  )

(def grammer 
  (rep+
    (conc
      expression
      (lit ";"))))

(defn lex [s]
   (filter (fn [x] (nil? (re-matches #" |\n|\r" x))) (re-seq #"." s)))

(defn parse [s]
  (rule-match grammer prn prn {:remainder (lex s)}))