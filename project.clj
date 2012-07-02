(defproject doubletake "1.0.0-SNAPSHOT"
  :description "A semantic interference detector for Java"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojars.doo/fnparse "2.2.8"]
                 [lein-javac "1.3.0"]
                 [lein-junit "1.0.0"]]
   :source-paths ["src/"]
   :javac-source-paths ["src/" "test/"]
   :javac-options {:debug "true"}
   :junit [["classes"]]
   :main doubletake.core)

