(ns doubletake.parser
  (:use ; std. lib.
        clojure.set
        ; this codebase
        doubletake.java.primitives
        doubletake.java.types
        ; 3rd-party
        name.choi.joshua.fnparse
        clojure.contrib.error-kit))

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

;-------------------------------------------------------------------------------
; LEXER AND PARSER
;-------------------------------------------------------------------------------

(def grammer)

(def splitter
  #"(\w+)|( )|([{}=\+\*\?:;\(\)\.])")

(defn lex [s]
  (map first (re-groups splitter s)))

(defn parser [s]
  (rule-match grammer prn prn {:remainder (lex s)}))
