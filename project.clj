(defproject doubletake "1.0.0-SNAPSHOT"
  :description "A semantic interference detector for Java"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [fnparse "2.2.7"]
                 [ordered "1.3.0"]]
  :source-paths ["src/"]
  :javac-source-paths ["src/"]
  :javac-options {:debug "true"})
