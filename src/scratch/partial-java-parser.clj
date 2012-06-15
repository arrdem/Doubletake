(ns doubletake.parser
  :use [clojure.string
        name.choi.joshua.fnparse 
        clojure.contrib.error-kit])

;------------------------------------------------------------------------------
; ERRORS
;------------------------------------------------------------------------------

(deferror parse-error [] [state message message-args]
  {:msg (str (format "JAVA parse error at line %s, column %s: "
               (:line state) (:column state))
             (apply format message message-args))
   :unhandled (throw-msg Exception)})

(defn- expectation-error-fn [expectation]
  (fn [remainder state]
    (raise parse-error state "%s expected where \"%s\" is"
      [expectation (or (first remainder) "the end of the file")])))

;------------------------------------------------------------------------------
; CHARACTER SETS
;------------------------------------------------------------------------------

(def alphabet-lower
  (alt
    (lit "a") (lit "b") (lit "c") (lit "d") (lit "e") (lit "f") (lit "g") 
    (lit "h") (lit "i") (lit "j") (lit "k") (lit "l") (lit "m") (lit "n")
    (lit "o") (lit "p") (lit "q") (lit "r") (lit "s") (lit "t") (lit "u")
    (lit "v") (lit "w") (lit "x") (lit "y") (lit "z")))

(def alphabet-upper
  (alt
    (lit "A") (lit "B") (lit "C") (lit "D") (lit "E") (lit "F") (lit "G")
    (lit "H") (lit "I") (lit "J") (lit "K") (lit "L") (lit "M") (lit "N")
    (lit "O") (lit "P") (lit "Q") (lit "R") (lit "S") (lit "T") (lit "U")
    (lit "V") (lit "W") (lit "X") (lit "Y") (lit "Z")))

(def digits
  (alt
    (lit "1") (lit "2") (lit "3") (lit "4") (lit "5") (lit "6") (lit "7")
    (lit "8") (lit "9") (lit "0")))

;------------------------------------------------------------------------------
; KEYWORDS AND LITTERALS
;------------------------------------------------------------------------------

(def keyword  
  "<keyword> ::= abstract | boolean | break | byte | case | catch | char | class
               | const | continue | default | do | double | else| extends 
               | final | finally | float | for | goto | if | implements 
               | import | instanceof | int | interface | long | native | new 
               | package | private | protected | public | return | short 
               | static | super | switch | synchronized | this | throw | throws
               | transient | try | void | volatile | while"
  (alt
    (lit "abstract") (lit "boolean") (lit "break") (lit "byte")
    (lit "case") (lit "catch") (lit "char") (lit "class")(lit "const")
    (lit "continue") (lit "default") (lit "do") (lit "double") (lit "else")
    (lit "extends") (lit "final") (lit "finally") (lit "float") (lit "for")
    (lit "goto") (lit "if") (lit "implements") (lit "import") 
    (lit "instanceof") (lit "int") (lit "interface") (lit "long")(lit "native")
    (lit "new") (lit "package") (lit "private") (lit "protected") (lit "public")
    (lit "return") (lit "short") (lit "static") (lit "super") (lit "switch")
    (lit "synchronized") (lit "this") (lit "throw") (lit "throws") 
    (lit "transient") (lit "try") (lit "void") (lit "volatile") (lit "while")))

(def null-literal
  "<null literal> ::= null"
  (lit "null"))

(def boolean-literal
  "<boolean literal> ::= true | false"
  (alt
    (lit "true:")
    (lit "false")))

;------------------------------------------------------------------------------
; PRODUCTION RULES
;------------------------------------------------------------------------------

(def identifier
  "<identifier>  ::= a..z,$,_ < a..z,$,_,0..9,unicode character over 00C0 > ."
  (conc
    (alt 
      alphabet-lower
      alphabet-upper
      (lit "_")
      (lit "$"))
    (rep*
      (alt 
        alphabet-lower
        alphabet-upper
        (lit "_")
        (lit "$")
        digits))))

(def super-class
  "<super> ::= extends <class type>"
  (conc
    (lit "extends "

(def class-modifier
  "<class modifier> ::= public | abstract | final"
  (alt
    (lit "public")
    (lit "final")
    (lit "abstract")))

(def class-declaration
  "<class declaration> ::= <class modifier>* class <identifier> <super>? <interfaces>? <class body>"
  (conc
    (rep* class-modifier)
    (lit "class ")
    identifier
    (rep< 1 super-class)
    (rep< 1 interfaces)
    class-body))

(def type-declaration
  "<type declaration> ::= <class declaration> | <interface declaration> | ;"
  (alt
    class-declaration
    interface-declaration
    (lit ";")))

(def type declarations
  "<type declarations> ::= <type declaration> | <type declarations> <type declaration>"
  (alt
    type-declaration
    (concat
      type-declaration
      type-declarations)))
  
(def wildcard-type-import-declaration
  "<wildcard type import declaration> ::= import <package name> . * ;"
  (conc
    (lit "import")
    package-name
    (lit ".*;")))

(def single-type-import-declaration
  "<single type import declaration> ::= import <type name> ;"
  (conc
    (lit "import")
    type-name
    (lit ";")))

(def import-declaration
  "<import declaration> ::= <single type import declaration> | <type import on demand declaration>"
  (alt
    single-type-import-declaration
    wildcard-type-import-declaration))

(def import-declarations
  "<import declarations> ::= <import declaration> | <import declarations> <import declaration>"
  (alt
    import-declaration
    (conc
      import-declaration
      import-declarations)))

(def package-name
  "<package name> ::= <identifier> | <package name> . <identifier>"
  (alt
    (conc
      identifier
      (lit ".")
      package-name)
    identifier))

(def package-declaration
  "<package declaration> ::= package <package name> ;"
  (conc
    (lit "package")
    package-name
    (lit ";")))

(def compilation-unit
  "<compilation unit> ::= <package declaration>? <import declarations>? <type declarations>?"
  (conc
    (rep< 1 (lit "package"))
    (rep< 1 import-declarations)
    (rep< 1 type-declarations)))

