(defproject betsim "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [incanter "1.5.1"]]
  :main          betsim.core
  :profiles     {:dev {:dependencies [[speclj "2.5.0"]]}}
  :plugins      [[speclj "2.5.0"]]
  :test-paths   ["spec/"])
