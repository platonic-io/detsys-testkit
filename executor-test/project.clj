(defproject executor-test "0.1.0-SNAPSHOT"
  :description "Simple executor to test detsys"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :main executor-test.core
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.priority-map "1.0.0"]
                 [ring/ring-core "1.8.1"]
                 [ring/ring-devel "1.8.1"]
                 [ring/ring-defaults "0.3.2"]
                 [http-kit "2.4.0"]
                 [metosin/jsonista "0.2.6"]
                 [metosin/reitit "0.5.5"]
                 [com.taoensso/timbre "4.10.0"]]
  :repl-options {:init-ns executor-test.core})
