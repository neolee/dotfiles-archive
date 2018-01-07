{:live-repl {:repl-options {:port 4555}}
 :user {:aliases {"live" ["with-profile" "default,live-repl" "repl"]}
        :dependencies [[alembic "0.3.2"]
                       [criterium "0.4.4"]
                       [org.clojure/tools.nrepl "0.2.12"]]
        :plugins [
                  [cider/cider-nrepl "0.17.0-SNAPSHOT"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]
                  [clj-http "3.7.0"]
                  [lein-ancient "0.6.12"]
                  [lein-marginalia "0.9.0"]
                  ]}
 }
