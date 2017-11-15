{:live-repl {:repl-options {:port 4555}}
 :user {:aliases {"live" ["with-profile" "default,live-repl" "repl"]}
        :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                       [criterium "0.4.4"]]
        :plugins [
                  [cider/cider-nrepl "0.16.0-SNAPSHOT"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]
                  [clj-http "3.7.0"]
                  [lein-ancient "0.6.12"]
                  [lein-marginalia "0.9.0"]
                  ]}
 }
