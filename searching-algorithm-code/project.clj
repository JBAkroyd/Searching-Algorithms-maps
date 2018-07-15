(defproject jba9_assignment2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"][org.clojure/data.priority-map "0.0.9"]]
  :main ^:skip-aot jba9-assignment2.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
