(defproject oroboros "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [swank-clojure "1.4.0-SNAPSHOT"]
                 [clojure-source "1.2.0"]
                 [penumbra "0.6.0-SNAPSHOT"]]
  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  :native-dependencies [[lwjgl "2.4.2"]]
  :dev-dependencies [[native-deps "1.0.5"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"})
