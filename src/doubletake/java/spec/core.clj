(ns doubletake.java.spec.core
  (:use clojure.set
        clojure.core
        clojure.stacktrace
        ordered.set
        doubletake.java.spec.parser))

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

(deftype spec-symbol [name depends rules])

(defn rule-name? [s]
  (re-find #"^[A-Z][a-zA-Z]*" s))

(defn make-spec-symbol [f]
  (spec-symbol. 
    (nth f 0) ; the name is the first element of the list
    (difference (set (filter rule-name? (flatten (nth f 1)))) (set (nth f 0)))
    (nth f 1)))

(defn make-spec-scope [raw-rules]
  (let [[rules] [(map make-spec-symbol raw-rules)]]
    (zipmap
      (map #(. % name) rules)
      rules)))

(defn print-node [node]
  (if (nil? node) 
    (println "called print-node with nil arguments")
    (print 
      (format "%s:\n%s\n\n"
              (. node name)
              (apply str 
                     (map #(str "   " (apply str %) "\n") 
                          (. node rules)))))))

(defn is-dep-edge [node resolved]
  (subset? (. node depends) resolved))

(defn is-dep-circular [node unresolved]
  (let [[common] [(intersection (. node depends) unresolved)]]
    (if (contains? unresolved (. node name)) common false)))

(defn is-dep-satisfiable [node scope]
  (reduce (fn [a b] (and a (contains? scope b))) true (. node depends)))

(defn try-resolve [scope edge resolved seen]
  (cond
    (is-dep-edge edge resolved)
      (do
        (conj resolved (. edge name)))
    (is-dep-circular edge seen)
      (throw (Exception. (str "Recursive dependancy exception.\n    "
                              (. edge name) 
                              " depends circularly upon: " 
                              (intersection seen 
                                            (. edge depends)))))
    (not (is-dep-satisfiable edge scope))
      (throw (Exception. (str "Undefined dependancy exception.\n    "
                              (. edge name) 
                              " depends upon: " 
                              (apply str 
                                     (interpose " " 
                                         (difference (. edge depends)
                                                     (set (map #(key %) scope)))))
                              " which is/are not defined\n")))
     true resolved))

(defn _make-resolve-order
  ([scope node resolved seen]
    (let [[node-obj iseen res] 
          [(get scope node)
           (conj seen node)
           (try-resolve scope (get scope node) 
                        resolved seen)
          ]]
      (if (contains? res node)
        res
        (recur scope node
               (reduce (fn [x y]
                         (pprint x)
                         (into (apply ordered-set (_make-resolve-order scope y x iseen)) [y]))
                       (conj resolved node) 
                       (difference (. node-obj depends) resolved (set [node])))
               seen)))))

(defn make-resolve-order [scope node]
  (reverse (_make-resolve-order scope node (ordered-set) (ordered-set))))

(defn print-resolve-order [scope order]
  (loop [head (first order) 
         tail (rest order)]
    (print-node (get scope head))
    (if (empty? tail) 
      nil
      (recur (first tail) 
             (rest tail)))))
    

(def testfile
  "/home/reid/Documents/work/DeWayne/java-doubletake/src/doubletake/java/spec/test.bnf")

(def g (parse testfile))
(def scope (make-spec-scope g))

