(ns doubletake.slicer
  (:use clojure.stacktrace
        clojure.pprint)
  ;(:require [doubletake.slicer-subtrees])
  ;(:import  [org.eclipse.jdt.core.dom ASTNode])
  )

(def ^:dynamic *dbg* false)

(defn zip [& seqs]
  (eval
    (concat '(map vector) seqs)))

(defn join [[& s]]
  (apply concat s))

(defn type? [o t]
  (= t (type o)))

(defn path-seq? [v]
  (and (coll? v)
       (or (= (first v) :alt)
           (= (first v) :seq))
       (not (empty? (rest v)))))

(defn prefix [s tails]
  (map #(concat s %1) tails))

(defn paths
  "About:
    Takes a sequence object as its only argument and using the following
    sequence structure specification generates the list of all possible paths
    down nested sequences befitting the spec.

  Sequence Spec:
    [meta & tail]
    All valid argmuent sequences S to this function will begin with a Clojure
    symbol which signifies whether the elements of the list represent alternate
    paths through the list or whether they represent a sequence to be included
    literally which may require recursion to expand into a full path.
    
    Valid Symbols:
      :seq - indicates that the list is a sequence which should be included but
             checked for recursion into other subtrees.

      :alt - indicates that the list is a sequence of sequences only one of
             which can be correctly included.

  Note:
    The nil literal if encountered in a sequence represents that an empty edge
    is a valid case of that sequence and causes nothing to be added to the path
    when evaluated.

  Example:
  (seq-paths [:seq :a [:alt :b1 :b2 :b3] :c [:seq :d [:alt :e1 nil]]])
  => [(:a :b1 :c :d :e1)
      (:a :b1 :c :d)
      (:a :b2 :c :d :e1)
      (:a :b2 :c :d)
      (:a :b3 :c :d :e1)
      (:a :b3 :c :d)]
  "
  [[sym & body]]
  (cond
    (= :alt sym)
        (reduce (fn [prev s]
                  (if (path-seq? s)
                      (concat prev (paths s))
                      (conj prev [s])))
                [] body)

    (= :seq sym)
        (reduce 
          (fn [partial-paths el]
            (if *dbg* (println "[0]" partial-paths el))
            (let [partial-tail (if (path-seq? el)
                                 (paths el)
                                 [[el]])]
              (reduce 
                (fn [finished-partials el]
                  (if *dbg* (println "[1]\n\t" partial-tail "\n\t" el))
                  (concat finished-partials
                          (map #(apply (fn [& x] (remove nil? x)) %1)
                            (prefix (if (seq? el) el (list el)) partial-tail))))
                [] partial-paths)))
          [nil] body)

    :else 
        (concat [sym] body)
    ))
