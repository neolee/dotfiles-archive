{:live-repl {:repl-options {:port 4555}}
 :user {:aliases {"live" ["with-profile" "default,live-repl" "repl"]}
        :dependencies [[alembic "0.3.2"]
                       [criterium "0.4.5"]
                       [org.clojure/tools.nrepl "0.2.13"]]
        :middleware [cider-nrepl.plugin/middleware
                     refactor-nrepl.plugin/middleware
                     ]
        :plugins [
                  [cider/cider-nrepl "0.22.3"]
                  [refactor-nrepl "2.5.0-SNAPSHOT"]
                  [clj-http "3.10.0"]
                  [lein-ancient "0.6.15"]
                  [lein-marginalia "0.9.1"]
                  ]}
 }
