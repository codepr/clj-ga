(defproject genetics "0.1.0-SNAPSHOT"
  :description "Basic genetic algorithm"
  :url "https://github.com/codepr/clj-ga"
  :license {:name "MIT"
            :url "https://github.com/codepr/clj-ga/LICENSE"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot genetics.core
  :target-path "target/%s"
  :profiles {:repl {:plugins [[cider/cider-nrepl "0.11.0"]]}
             :uberjar {:aot :all}})
