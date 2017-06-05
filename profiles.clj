{:live-repl {:repl-options {:port 4555}}
 :user {:aliases {"live" ["with-profile" "default,live-repl" "repl"]}
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [criterium "0.4.4"]]
        :plugins [
                  [cider/cider-nrepl "0.14.0"]
                  [refactor-nrepl "2.3.0-SNAPSHOT"]
                  [lein-ancient "0.6.10"]
                  [lein-clean-m2 "0.1.2"]
                  [lein-marginalia "0.9.0"]
                  ]}
 }
