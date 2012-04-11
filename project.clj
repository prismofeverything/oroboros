(defproject oroboros "1.0.0-SNAPSHOT"
  :description "oroboros - a self chasing homeostatic encirclement"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/core.incubator "0.1.0"]
                 ;; [clojure-source "1.2.0"]
                 [clj-glob "1.0.0"]
                 ;; [overtone/scsynth-jna "0.1.2-SNAPSHOT"]
                 ;; [overtone/at-at "0.2.0"]
                 ;; [overtone/osc-clj "0.7.0"]
                 ;; [overtone/byte-spec "0.3.0"]
                 ;; [overtone/midi-clj "0.2.0"]
                 [overtone "0.6.0"]
                 [incanter "1.2.2" :exclusions [swank-clojure]]
                 [penumbra "0.6.0"]]
  :native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [[native-deps "1.0.5"]
                     [clojure-source "1.2.0"]
                     [swank-clojure "1.4.2"]])

