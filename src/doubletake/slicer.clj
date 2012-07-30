(ns doubletake.slicer)

(defn zip [& seqs]
  (eval
    (concat '(map vector) seqs)))

(defn join [[& s]]
  (apply concat s))

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
      ; the alternates case first...
      (list? head)
          (recur 
            (map join
                 (map 



