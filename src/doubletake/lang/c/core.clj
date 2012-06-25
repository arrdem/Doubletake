(ns doubletake.lang.c.core
  (:use
    [doubletake.lang.c.parser]
    [doubletake.parser.util :as util]))

(defn _processor [ast]
  ; takes an AST as its only parameter and from it produces the lists of def
  ; and use pairs by block which the Doubletake core relies upon.
  )

(defn parse [input]
  ; takes a raw string of text as input, and parses it into an AST
  (util/parser ???
               (util/prep (util/re-all-matches util/splitter
                                               input))))

(defn process [file]
  ; takes a file path or file object as its argument, and executes the entire
  ; lex/parse/process process on the provided target.
  (let [text (if (file? file) (slurp file) file)]
    (_processor (parse text))))
