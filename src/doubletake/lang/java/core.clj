(ns doubletake.lang.java.core
  (:use
    [clojure.set]
    [doubletake.lang.java.syntax]
    [doubletake.parser.util :as util]
    [clojure.contrib.java-utils :as jutil])
  
  (:import
    [doubletake.lang.java.Visitor]
    [doubletake.lang.java.Variable]))

(defn make-pairs [var]
  ; takes a Variable object and recursively generates the set of def/use/name
  ; tripples using the data from the var's member fields
  (loop [defs (. var defs)
         uses (. var uses)
         tripples (seq)]
    (cond
      (empty? uses) 
        tripples
        
      (>= (nth uses 0) (nth defs 0)) 
        (recur (rest defs) uses tripples)
        
      (>= (first uses) (first defs)) 
        (recur defs 
               (rest uses) 
               (conc tripples 
                     (Tripple. (first defs)
                               (first uses)
                               (. var identifier))
                     )
               )
        )
    )
  )

(defn process [f]
  ; takes a file path or file object as its argument, and executes the entire
  ; lex/parse/process process on the provided target.
  (let [text (slurp f)]
    (let [vis (. Visitor run text)]
      (set
        (reduce conc
                (map make-pairs 
                     (vals (. vis data))))))))
