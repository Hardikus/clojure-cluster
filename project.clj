(defproject matonthecat.clojure/clojure-cluster "0.1.0-SNAPSHOT"
    :description "Clustering algorithms for Clojure"  
    :source-paths ["src/main/clojure"]
    :test-paths  ["src/test/clojure"]
;    :dev-dependencies []
    :dependencies [[org.clojure/clojure "1.5.0-alpha3"]
                   [incanter/incanter-core "1.4.1"]
                   [org.clojure/clojure-contrib "1.2.0" :scope "test"]]
    :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"})