(defproject concurtimes "0.1.0-SNAPSHOT"
  :description "Printing text in justified columns. An exercise for Concur."
  :url "http://tiltontec.com"
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.385"]
                 [com.taoensso/timbre "4.3.1"]
                 [org.clojure/tools.cli "0.3.5"]]
  :main ^:skip-aot concurtimes.core
  :target-path "target/%s"
  :bin {:name "concurtimes"
        :bin-path "./bin"}
  :profiles {:uberjar {:aot :all}})
