(ns doubletake.lang.java.core
  (:use
    [doubletake.lang.java.syntax]
    [doubletake.parser.util :as util]
    [clojure.contrib.java-utils :as jutil]))

(defn _processor [ast]
  ; takes an AST as its only parameter and from it produces the lists of def
  ; and use pairs by block which the Doubletake core relies upon.
  )

(defn lex [input] 
  (util/prep util/banned 
             (re-seq #"(\w+)|." input)))

(defn parse 
  ([input rule]
  ; takes a raw string of text as input, and parses it into an AST
  (util/parser rule (lex input)))
  ([input] (parse input CompilationUnit)))

(defn process [f]
  ; takes a file path or file object as its argument, and executes the entire
  ; lex/parse/process process on the provided target.
  (let [text (if (. (jutil/file f) exists) (slurp f) f)]
    (_processor (parse text))))
