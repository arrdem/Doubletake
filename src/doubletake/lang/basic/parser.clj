(ns doubletake.lang.basic.parser
  (:use
    ; stdlib
    clojure.set
    ; third party
    name.choi.joshua.fnparse))

(def NewLine
  (alt
    (conc (lit "\r") (lit "\f"))
    (lit "\n")))

(def Whitespace
  (rep+ (alt (lit " ") (lit "\t"))))

(def Remark
  (conc (lit "REM") Whitespace (rep* (except anything NewLine)) NewLine))

(def ID
  (conc
    (term #(re-matches #"[a-zA-Z]+" %))
    (opt (lit "$"))))

(def BString
  (semantics
    (conc
      (lit "\"")
      (rep* (except anything (lit "\"")))
      (lit "\""))
    #(apply str (interpose " " (filter (fn [x] (not (= x "\""))) %)))))

(def BInteger
  (semantics (term #(re-matches #"[0-9]+" %)) #(Integer. %)))
   
(def BReal
  (semantics
    (conc
      (rep+
        (lit-alt-seq (seq "1234567890")))
      (lit ".")
      (rep+
        (lit-alt-seq (seq "1234567890"))))
    #(Double. %)))

(def Constant
  (alt
    BString
    BInteger
    BReal))

(def Expression)
(def Expression-List)

(def Value
  (alt
    (conc (lit "(") Expression (lit ")"))
    (conc ID (lit "(") Expression-List (lit ")"))
    ID
    Constant))

(def Power-Exp
  (alt
    (conc Power-Exp (lit "^") Value)
    Value))

(def Negate-Exp
  (alt
    (conc (lit "-") Power-Exp)
    Power-Exp))

(def Mult-Exp
  (alt
    (conc Negate-Exp (lit "*") Mult-Exp)
    (conc Negate-Exp (lit "/") Mult-Exp)
    Negate-Exp))

(def Add-Exp
  (alt
    (conc Mult-Exp (lit "+") Add-Exp)
    (conc Mult-Exp (lit "-") Add-Exp)
    Mult-Exp))

(def Compare-Exp
  (alt
    (conc Add-Exp (lit "==") Compare-Exp)
    (conc Add-Exp (lit "<>") Compare-Exp)
    (conc Add-Exp (lit "><") Compare-Exp)
    (conc Add-Exp (lit ">") Compare-Exp)
    (conc Add-Exp (lit ">=") Compare-Exp)
    (conc Add-Exp (lit "<") Compare-Exp)
    (conc Add-Exp (lit "<=") Compare-Exp)
    Add-Exp))

(def Not-Exp
  (alt
    (conc (lit "NOT") Compare-Exp)
    Compare-Exp))

(def And-Expr
  (alt
    (conc Not-Exp (lit "AND") And-Expr)
    Not-Exp))

(def Expression
  (alt
    (conc And-Expr (lit "OR") Expression)
    And-Expr))

(defmacro foo-list [foo sep]
  (conc
    foo
    (rep* (conc sep foo))))

(def Constant-List
  (foo-list Constant (lit ",")))

(def Access
  (alt
    (lit "INPUT")
    (lit "OUTPUT")))

(def ID-List
  (foo-list ID (lit ",")))

(def Value-List
  (foo-list Value (lit ",")))

(def Constant-List
  (foo-list Constant (lit ",")))

(def Integer-List
  (foo-list BInteger (lit ",")))

(def Expression-List
  (foo-list Expression (lit ",")))

(def Print-list
  (foo-list Expression (lit ";")))

(def Statements
  (foo-list Statement (lit ":")))

(def Statement
  (alt
    (conc (lit "CLOSE") (lit "#") BInteger)
    (conc (lit "DIM") ID Constant-List)
    (lit "END")
    (conc (lit "FOR") ID (lit "=") Expression (lit "TO") Expression (opt (conc (lit "STEP") BInteger)))
    (conc (lit "GOTO") (alt BInteger ID))
    (conc (lit "GOSUB") Expression)
    (conc (lit "IF") Expression (lit "THEN") Statement)
    (conc (lit "INPUT") ID-List)
    (conc (lit "OPEN") Value (lit "FOR") Access (lit "AS") (lit "#") BInteger)
    (conc (lit "POKE") Value-List)
    (conc (lit "PRINT") Print-list)
    (conc (lit "PRINT") (lit "#") BInteger (lit ",") Print-list)
    (conc (lit "READ") ID-List)
    (lit "RETURN")
    (lit "RESTORE")
    (lit "RUN")
    (lit "STOP")
    (conc (lit "SYS") Value)
    (conc (lit "WAIT") Value-List)
    Remark))

(def Lines
  (rep+
    (conc BInteger Statement NewLine)))

