(ns doubletake.lang.test.java
  (:use
    [clojure.pprint]
    [doubletake.lang.java.core :as jcore]
    [doubletake.lang.java.syntax :as jsyntax]
    [clojure.test]))

(defmacro java-parser-test [string]
  ;`(print ~string "\n\n")
  `(is (not (nil? (jcore/parse ~string)))))

(deftest basic-package
  (java-parser-test "package Foo;\n"))

(deftest package-with-import
  (java-parser-test
    (str "package Foo;\n"
         "import Clojure.Awesome.Functional;\n")))

(deftest package-with-wildcard-import 
  (java-parser-test
    (str "package Foo;\n"
         "import Clojure.Awesome.Functional;\n"
         "import Foo.Bar.Baz.*;\n")))

(deftest empty-class
  (java-parser-test 
    (str "class Bar {}\n")))

(deftest class-with-modified-fields 
  (java-parser-test 
    (str "class Bar {\n"
         "public int a;\n"
         "protected int b;\n"
         "private static int i;\n"
         "}\n")))

(deftest class-with-member-function
  (java-parser-test
    (str "class A_Stupid_Name {\n"
         "    public int tarded;\n"
         "    public A_Stupid_Name() {\n"
         "        this.tarded = 5;\n"
         "    }\n"
         "}\n")))
