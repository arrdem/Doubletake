(ns doubletake.lang.java.core
  (:use
    [clojure.set]
    [clojure.contrib.java-utils :as jutil])
  
  (:import
    [doubletake.lang.java Visitor Variable Tripple]))

(defn make-pairs [var]
  ; takes a Variable object and recursively generates the set of def/use/name
  ; tripples using the data from the var's member fields
  (loop [defs (. var defs)
         uses (. var uses)
         tripples []]
    (cond
      (empty? uses) 
        tripples
        
      (>= (nth uses 0) (nth defs 0)) 
        (recur (rest defs) uses tripples)
        
      (>= (first uses) (first defs)) 
        (recur defs 
               (rest uses) 
               (concat tripples
                     (Tripple. (first defs)
                               (first uses)
                               (. var identifier))
                     )
               )
        )
    )
  )

(defn gen-tripples [vis]
  (set
    (reduce concat
            (map make-pairs 
                 (vals (. vis (getData) ))))))

(defn process-text [t]
  (let [vis (. Visitor run t)]
    (gen-tripples vis)))

(defn process-file [f] 
  (let [text (slurp f)]
    (process-text text)))

