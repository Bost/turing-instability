(defproject turing-instability "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [ring/ring-core "1.0.0"]
                 [ring/ring-jetty-adapter "1.0.0"]
                 [hiccup "1.0.0-beta1"]
                 [compojure "1.0.1"]
                 ;[com.keminglabs/c2 "0.0.1-SNAPSHOT"]    ; TODO try it when a stable 1.0 release comes out (May 2012)
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [org.clojars.pallix/analemma "1.0.0-SNAPSHOT"]
                 ]

:main turing-instability.core
  ;; This namespace will get loaded automatically when you launch a repl.
				 )

;(defn -main [& args]
;  ...)
