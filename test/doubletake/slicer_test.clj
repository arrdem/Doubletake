(ns doubletake.slicer-test
  (:use doubletake.slicer
        clojure.test
        clojure.stacktrace))

(def t1-input [:seq :a [:alt :b1 :b2 :b3] :c [:seq :d [:alt :e1 nil]]])
(def t1-output
  [(list :a :b1 :c :d :e1)
   (list :a :b1 :c :d)
   (list :a :b2 :c :d :e1)
   (list :a :b2 :c :d)
   (list :a :b3 :c :d :e1)
   (list :a :b3 :c :d)])

(deftest testone
  (testing 
    "The sample case provided in the documentation ofdoubletake.slicer/paths"
    (is (= (paths t1-input) t1-output))))
