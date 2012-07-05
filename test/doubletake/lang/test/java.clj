(ns doubletake.lang.test.java
  (:use 
    [clojure.test]
    [clojure.pprint]
    [doubletake.lang.java.core])
  
  (:import
    [doubletake.lang.java Tripple Visitor Variable]))

;-------------------------------------------------------------------------------
; First define some basic tests for Visitor.run
;-------------------------------------------------------------------------------

(defn vtest [code]
  (is (not (nil? (. Visitor run code)))))

(deftest simple-class
  (vtest
    (str "public class A {\n"
		     "  int i = 9;\n"
		     "  int j;\n"
		     "  ArrayList<Integer> al = new ArrayList<Integer>();\n"
		     "  j=1000;\n"
		     "}")))

(deftest class-with-member-f
   (vtest
    (str "public class YourMother {\n"
         "    private int i, j, k;\n"
         "    public int getVar(char c) {\n"
         "        switch(c) {\n"
         "            case 'i':\n"
         "                return this.i;\n"
         "            case 'j':\n"
         "                return this.j;\n"
         "            case 'k':\n"
         "                return this.k;\n"
         "            default:\n"
         "                return 9001;\n"
         "        }\n"
         "    }\n"
         "}\n")))


(deftest class-with-member-call
  (vtest
    (str "public class YourMother {\n"
         "    private int i=1,j=2,k=3;\n"
         "    public static void main() {\n"
         "        int l = 0;\n"
         "        l = getVar('a');\n"
         "        System.out.println(l);\n"
         "    }\n"
         "    public int getVar(char c) {\n"
         "        switch(c) {\n"
         "            case 'i':\n"
         "                return this.i;\n"
         "            case 'j':\n"
         "                return this.j;\n"
         "            case 'k':\n"
         "                return this.k;\n"
         "            default:\n"
         "                return 9001;\n"
         "        }\n"
         "    }\n"
         "}\n")))

;-------------------------------------------------------------------------------
; Define some tests for the Tripple generation code.
;-------------------------------------------------------------------------------


(deftest simple-gen-tripple
   (let [vis 
         (. Visitor run (str "public class A {\n"
                             "  int i = 9;\n"
                             "  int j;\n"
                             "  ArrayList<Integer> al = new ArrayList<Integer>();\n"
                             "  j=1000;\n"
                             "}"))] 
    (pprint (gen-tripples vis))))
  
