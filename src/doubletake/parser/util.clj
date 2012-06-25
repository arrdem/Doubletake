(ns doubletake.parser.util
   (:use ; std. lib.
        clojure.set
        ; 3rd-party
        name.choi.joshua.fnparse
        clojure.contrib.error-kit))

; This file provides some tools for lanuage parsers and lexers which I (or
; anyone else who wants to write a language module) would otherwise have
; to re-write for ever freaking time.

(defmacro join-semantics [rule]
  (semantics rule
             #(apply str %)))

(defn re-all-matches [pattern string]
  (let [matcher (re-matcher pattern string)]
    (loop [match  (re-find matcher)
           result []]
      (if (nil? match)
        result
        (do
          (recur (re-find matcher) (concat result [match])))))))

(def banned #{" " "\t"})

(defn prep [banned tokens]
  ; takes the result of re-all-matches and cleans it into a stream of single
  ; tokens which fnparse can consume.
  (filter #(not (contains? banned %))
          (map #(first %)
               tokens)))

(def splitter 
  #"(\w+)|(.)")

(defn lex [s] 
  (map first (re-all-matches splitter s)))

(defn parser [grammar tokens]
  (rule-match grammar prn prn {:remainder tokens}))




