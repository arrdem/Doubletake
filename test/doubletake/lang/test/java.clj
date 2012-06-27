(ns doubletake.lang.test.java
  (:use
    [clojure.pprint]
    [doubletake.lang.java.core :as jcore]
    [doubletake.lang.java.syntax :as jsyntax]
    [clojure.test]))

(defmacro java-parser-test [test-name string]
  (deftest test-name 
           (quote
             (println "-------------------------------------------------")
             (print string "\n\n")
             (let [res (jcore/parse string)]
               (is (not (= nil res)))
               (pprint res)))))

(java-parser-test basic-package "package Foo;\n")


(java-parser-test package-with-import 
                  (str "package Foo;\n"
                       "import Clojure.Awesome.Functional;\n"))

(java-parser-test package-with-wildcard-import 
                 (str "package Foo;\n"
                      "import Clojure.Awesome.Functional;\n"
                      "import Foo.Bar.Baz.*;\n"))


(java-parser-test empty-class (str "class Bar {}\n"))

(java-parser-test class-with-modified-fields 
                 (str "class Bar {\n"
                      "public int a;\n"
                      "protected int b;\n"
                      "private static int i;\n"
                      "}\n"))


(java-parser-test class-with-member-function 
                  (str "class A_Stupid_Name {\n"
                       "    public int tarded;\n"
                       "    public A_Stupid_Name() {\n"
                       "        this.tarded = 5;\n"
                       "    }\n"
                       "}\n"))
