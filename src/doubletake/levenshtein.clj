(ns doubletake.levenshtein)
; This file provides two implementations of the Levenshtein edit distance
; algorithm, one of which simply calculates the length of the shortest edit
; script and the other of which actually yields an edit script.

(declare levenshtein-distance levenshtein-script)

(defn cost [a b]
  (if (= a b) 0 1))

(defn _levenshtein-distance
  "About:
    Calculates the edit-distance between two sequences.

  Arguments:
    Two objects implementing Clojure's ISeq interface."
  [seq1 seq2]
  (cond
    (empty? seq1) (count seq2)
    (empty? seq2) (count seq1)
    :else (min
           (+ (cost (first seq1) (first seq2)) (levenshtein-distance (rest seq1) (rest seq2))) ;; substitution
           (inc (levenshtein-distance (rest seq1) seq2))    ;; insertion
           (inc (levenshtein-distance seq1 (rest seq2)))))) ;; deletion

(def levenshtein-distance (memoize _levenshtein-distance))

(defn _levenshtein-script
  "About:
    Calculates the edit-script between two sequences, being a list of pairs
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
                             (levenshtein-script (rest seq1) (rest seq2)))
                    sub-len (count sub)
  
                    ins (cons (list :insert (first seq2))
                              (levenshtein-script seq1 (rest seq2)))
                    ins-len (count ins)

                    del (cons (list :delete (first seq1))
                            (levenshtein-script (rest seq1) seq2))
                    del-len (count del)]
                (cond 
                  (= sub-len (min sub-len ins-len del-len)) sub
                  (= ins-len (min sub-len ins-len del-len)) ins
                  :else del)))))

(def levenshtein-script (memoize _levenshtein-script))
