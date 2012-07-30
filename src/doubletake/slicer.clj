(ns doubletake.slicer
  (:require [doubletake.slicer-subtrees])
  (:import  [org.eclipse.jdt.core.dom ASTNode]))

(defn zip [& seqs]
  (eval
    (concat '(map vector) seqs)))

(defn join [[& s]]
  (apply concat s))

(defn type? [o t]
  (= t (type o)))

(defn seq-paths
 "About:
    Takes a sequence as its only argument consisting of nested objects and
    sequences. The type of the nested sequences is used to determine whether it
    is supposed to represent a series of objects which should be included
    simultaneously in a slice path or whether it constitutes a list of
    alternate paths.
  Note:
    The \"list\" type is used to denote a series of alternate paths.
    The \"vector\" type is used to denoote a series of \"statements\".
    The type of the outermost argument sequence is ignored.
  "
  [arg-seq]
  (loop [slices [[]]
         head   (first arg-seq)
         tail   (rest arg-seq)]
    (cond
      ; the terminal case...
      (and (nil? head) (empty? tail))
          slices

      ; the alternates case...
      (list? head)
          ; In this case map seq-paths over the list of alternates to get the
          ; list of lists of a
          (let [n (map seq-paths head)]
            (recur
              (reduce (fn [prev x] 
                        (concat prev (map #(concat % x) slices)))
                      [] n)
              (first tail)
              (rest tail)))


      ; the sequence case...
      (vector? head)
          ; This case is pretty simillar to the alternate case, in that it
          ; requires calculating the seq-paths of the argument list. Note that
          ; here we care about the actual seq-paths evaluation OF THE ENTIRE
          ; VECTOR not the seq-paths of the individual elements.
          (let [n (apply seq-paths head)]
            (recur (map (fn [s] (map #(concat s %) n)) slice)
                   (first tail)
                   (rest tail)))

      ; the ASTNode case...
      (type? head ASTNode)
          ; This case is used to evaluate the subtrees of the targeted node
          ; when slicing paths through and Eclipse AST as this code is intended
          ; to do.
          (let [st (subtrees head)]
            (recur slices
                   (if (or (nil? st) (empty? st)) head st)
                   tail))

      ; the atom case...
      :else
            ; This case is used as the default and should be hit only in
            ; testing.
         (recur (map #(conj % head) slices)
                (first tail)
                (rest tail)))))

