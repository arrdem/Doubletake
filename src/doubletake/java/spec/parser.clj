(ns doubletake.java.spec.parser
  (:use ; std. lib.
        clojure.set
        clojure.pprint
        ; 3rd-party
        name.choi.joshua.fnparse))

(def rule-name 
  ; used to match the name of a rule, since CammelCase is used, the first char
  ; will be capitalized and that is a sufficient criteria for a name
  (term #(re-find #"[A-Z][a-zA-Z]*" %)))

(def indent
  ; used to match the indentation denoting the continuation of a rule
  ; three (3) spaces in the JFL
  (alt
    (lit "    ")
    (lit "   ")
    (lit "\t")))

(def my-newline
  (lit "\n"))

(def rule-end
  (rep= 2 my-newline))

(def operator
  ; used to recognize the opt token which signifies that the proceeding token
  ; may or may not appear
  (lit "opt"))

(def pattern
  ; used to recognize REGEX patterns in the Clojure syntax therefor
  (semantics
    (conc
      (lit-conc-seq ["#" "\""])
      (rep+
        (except
          anything
          (lit "\"")))
        (lit "\""))
    #(apply str (flatten %))))

(def literal
  ; used to recognize a literal character or string.
  anything)

(def my-comment
  ; matches a line starting with the ; character
  (semantics 
    (conc
      (lit ";")
      (semantics (rep* (term #(re-find #"[^\n]" %))) #(apply str %))
      (lit "\n"))
    (fn [x]
      (print x)
      "\n")))

(def match-expression
  (semantics
    (conc
      (rep+ (except (alt pattern anything) my-newline))
      my-newline)
    #(first %)))
  

(def rule
  (semantics
    (conc rule-name 
          (lit ":") 
          my-newline
          (rep+ 
            (semantics
              (conc indent 
                    match-expression)
            #(nth % 1)))
        my-newline)
    #(let [[a][%]] (list (nth a 0) (nth a 3)))))
    

(def grammer (rep* (alt my-comment rule my-newline)))

(defn lex [s]
  (map first (re-seq #"(opt)|(\w+)|(    )|(\n)|(.)" s)))

(defn parser [s]
  (rule-match grammer prn prn {:remainder (lex s)}))

(defn parse [fname]
  (filter #(not (= "\n" %)) (parser (slurp fname))))