(defproject advent-of-code-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.rpl/specter "1.1.3"]
                 [meander/epsilon "0.0.650"]
                 [datascript "1.3.1"]
                 [net.mikera/core.matrix "0.62.0"]
                 [ubergraph "0.8.2"]
                 [criterium "0.4.6"]
                 [com.clojure-goes-fast/clj-async-profiler "0.5.1"]]
  :main ^:skip-aot advent-of-code-clj.core
  :target-path "target/%s"
  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
