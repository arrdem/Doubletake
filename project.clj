(defproject doubletake "1.0.0-SNAPSHOT"
  :description "A semantic interference detector for Java"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [fnparse "2.2.7"]]
  :source-path [["src/"]]
  :javac-source-path [["src/"]]
  :javac-options {:debug "true"}
  :main doubletake.core)
