(ns doubletake.java.spec.core
  (:use clojure.set
        clojure.core
        doubletake.java.spec.parser)
  (:import doubletake.java.spec.DependTree))

; python dependancy resolution sample code from
; http://www.electricmonk.nl/log/2008/08/07/dependency-resolving-algorithm/
; needs some work for conversion to Clojure, but the idea is all there.
;
; class Node:
;    def __init__(self, name):
;       self.name = name
;       self.edges = []
;  
;    def addEdge(self, node):
;       self.edges.append(node)
;
; def dep_resolve(node, resolved, unresolved):
;    unresolved.append(node)
;    for edge in node.edges:
;       if edge not in resolved:
;          if edge in unresolved:
;             raise Exception('Circular reference detected: %s -> %s' % (node.name, edge.name))
;          dep_resolve(edge, resolved, unresolved)
;    resolved.append(node)
;    unresolved.remove(node)

(defn rule-name? [s]
  (re-find #"^[A-Z][a-zA-Z]*" s))

(defn make-spec-symbol [f]
  (DependTree. 
    (nth f 0) ; the name is the first element of the list
    (difference (set (filter rule-name? (flatten (nth f 1)))) (set (nth f 0)))
    (nth f 1)))

(defn make-spec-scope [raw-rules]
  (let [[rules] [(map make-spec-symbol raw-rules)]]
    (zipmap
      (map #(. % name) rules)
      rules)))

(defn print-node [node]
  (print 
    (format "%s:\n%s"
            (. node name)
            (apply str 
                   (map #(str "   " (apply str %) "\n") 
                        (. node cases))))))

(defn dep-resolve [scope node]
   (.resolveDepends DependTree scope node)) 

(def testfile
  "/home/reid/Documents/work/DeWayne/java-doubletake/src/doubletake/java/spec/test.bnf")

(def g (parse testfile))
(def scope (make-spec-scope g))
