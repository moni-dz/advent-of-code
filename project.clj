(defproject advent-of-code "1.0"
  :dependencies [[org.clojure/clojure "1.13.0-master-SNAPSHOT"]
                 [org.clojure/core.async "1.7.701"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [criterium/criterium "0.4.6"]
                 [tech.droit/fset "0.1.1"]]
  :plugins [[lein-exec "0.3.7"]
            [lein-ancient "1.0.0-RC3"]
            [jonase/eastwood "1.4.3"]]
  :source-paths ["src"]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"})
