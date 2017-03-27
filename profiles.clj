{:live-repl {:repl-options {:port 4555}}
 :user {:aliases {"live" ["with-profile" "default,live-repl" "repl"]}
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [criterium "0.4.4"]]
        :plugins [
                  ; [cider/cider-nrepl "0.13.0"]
                  [cider/cider-nrepl "0.14.0"]
                  ; [refactor-nrepl "2.2.0"]
                  [refactor-nrepl "2.3.0-SNAPSHOT"]
                  [lein-marginalia "0.9.0"]
                  ]}
 }
