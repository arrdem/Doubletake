(defproject 
  doubletake "1.0.0-SNAPSHOT"

  :description 
            "A semantic interference detector for Java"

  :dependencies 
            [[org.clojure/clojure "1.3.0"]
             [org.clojars.doo/fnparse "2.2.8"]
             [lein-javac "1.3.0"]
             [clojure-contrib "1.2.0"]
             [lein-junit "1.0.0"]]

  :dev-dependencies 
            [[lein-javac "1.2.1-SNAPSHOT"]]

  :source-paths 
            ["src/"]

  :javac-source-paths 
            [["src/"]
             ["test/" :debug "true"]]

  :javac-options 
            {:debug "true"}

  :junit 
            [["classes"]]

  :main doubletake.core)
