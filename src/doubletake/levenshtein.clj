(ns doubletake.levenshtein)
;------------------------------------------------------------------------------
; This file provides two implementations of the Levenshtein edit distance
; algorithm, one of which simply calculates the length of the shortest edit
; script and the other of which actually yields an edit script.

(declare levenshtein-distance-slow levenshtein-script-slow)

(defn cost [a b]
  (if (= a b) 0 1))

(defn _levenshtein-distance-slow
  "About:
    Calculates the edit-distance-slow between two sequences.

  Arguments:
    Two objects implementing Clojure's ISeq interface."
  [seq1 seq2]
  (cond
    (empty? seq1) (count seq2)
    (empty? seq2) (count seq1)
    :else (min
           (+ (cost (first seq1) (first seq2)) (levenshtein-distance-slow (rest seq1) (rest seq2))) ;; substitution
           (inc (levenshtein-distance-slow (rest seq1) seq2))    ;; insertion
           (inc (levenshtein-distance-slow seq1 (rest seq2)))))) ;; deletion

(def levenshtein-distance-slow (memoize _levenshtein-distance-slow))

(defn _levenshtein-script-slow
  "About:
    Calculates the edit-script-slow between two sequences, being a list of pairs
    (:insert     <val-new>)
    (:delete     <val-old>)
    (:substitute <val-old> <val-new>)
    where val-old is the changed value from the original sequence and
    where val-new is the new value in the second sequence.

  Arguments:
    Two objects implementing Clojure's ISeq interface.
    seq1 represents the source sequence, and seq2 is the destination sequence."
  [seq1 seq2]
  (cond
    (empty? seq1) (map #(list :insert %) seq2)
    (empty? seq2) (map #(list :delete %) seq1)
    :else
      (remove nil?
              (let [sub (cons (if (= (first seq1) (first seq2))
                                nil (:substitute (first seq1) (first seq2)))
                             (levenshtein-script-slow (rest seq1) (rest seq2)))
                    sub-len (count sub)
  
                    ins (cons (list :insert (first seq2))
                              (levenshtein-script-slow seq1 (rest seq2)))
                    ins-len (count ins)

                    del (cons (list :delete (first seq1))
                            (levenshtein-script-slow (rest seq1) seq2))
                    del-len (count del)]
                (cond 
                  (= sub-len (min sub-len ins-len del-len)) sub
                  (= ins-len (min sub-len ins-len del-len)) ins
                  :else del)))))

(def levenshtein-script-slow (memoize _levenshtein-script-slow))

;------------------------------------------------------------------------------
; An anternate and hopefully much faster implementation based instead on the
; edit matrix calculation

(def Infinity (. Double POSITIVE_INFINITY))

(defmacro inf-fail [expr]
  `(try
    ~expr
    (catch IndexOutOfBoundsException e# Infinity)))

(defn array-2d-long [x y]
  (into-array (map long-array (map (fn [a] (repeat x 0)) (range y)))))

(defn aget-2d [a x y]
  (aget (aget a y) x))

(defn aset-2d [a x y v]
  (aset (aget a y) x v))

(defstruct levenshtein-data :matrix :x-max :y-max)

(defn levenshtein-calculation [seq1 seq2]
  (let [matrix (array-2d-long (inc (count seq1)) (inc (count seq2)))]
    (doseq [x (range (inc (count seq1))) y [0]]
      (aset-2d matrix x y x))
    (doseq [x [0] y (range (inc (count seq2)))]
      (aset-2d matrix x y y))
    (doseq [y (range 1 (inc (count seq2)))
            x (range 1 (inc (count seq1)))]
      (aset-2d matrix x y
               (cond
                 (= (nth seq1 (dec x)) (nth seq2 (dec y))) 
                    (aget-2d matrix (dec x) (dec y))
                 :else
                     (inc (min (aget-2d matrix (dec x) y)
                               (aget-2d matrix x (dec y))
                               (aget-2d matrix (dec x) (dec y)))))))
    (struct levenshtein-data 
            matrix
            (inc (count seq1))
            (inc (count seq2))
            )))

(defn levenshtein-data->script [{:keys [matrix x-max y-max]}]
  (loop [x x-max 
         y y-max
         script []]
    (let [_   (inf-fail (aget-2d matrix x y))
          del (inf-fail (aget-2d matrix (dec x) y))
          ins (inf-fail (aget-2d matrix x (dec y))) 
          sub (inf-fail (aget-2d matrix (dec x) (dec y)))
          m   (min del ins sub)]
      (cond
        (= 0 x y) (butlast script)
        (= m sub) (recur (dec x) (dec y) 
                         (if (= _ sub) 
                           script
                           (cons (list :sub (dec x) (dec y)) script))) 
        (= m ins) (recur x (dec y) (cons (list :ins (dec y)) script))
        (= m del) (recur (dec x) y (cons (list :del (dec x)) script))))))
