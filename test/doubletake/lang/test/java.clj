(ns doubletake.lang.test.java
  (:use
    [clojure.pprint]
    [doubletake.lang.java.core :as jcore]
    [doubletake.lang.java.syntax :as jsyntax]
    [clojure.test]))

(def test-strings
  ["class Point { int x, y; }\n"
   "package vista;\nclass Point { int x, y; }\n"
   "package vista;\nimport java.util.Vector;\nclass Point { int x, y; }\n"])

(deftest string-parse
  (loop [cur  (first test-strings)
         tail (rest test-strings)]
    (let [res (jcore/parse cur)]
      (is (not (= nil res)))
      (println "-------------------------------------------------")
      (print cur "\n\n")
      (pprint res)
      (if (not (empty? tail))
        (recur (first tail) (rest tail))))))

