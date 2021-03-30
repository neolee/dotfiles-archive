{:live-repl {:repl-options {:port 4555}}
 :uberjar {:aot :all}
 :user {:aliases {"live" ["with-profile" "default,live-repl" "repl"]
                  "native" ["shell"
                            "native-image" "--report-unsupported-elements-at-runtime"
                            "--initialize-at-build-time"
                            "-jar" "./target/${:uberjar-name:-${:name}-${:version}-standalone.jar}"
                            "-H:Name=./target/${:name}"]
                  }
        :dependencies [[clojure-emacs/alembic "0.3.3"]
                       [criterium "0.4.6"]
                       [org.clojure/tools.nrepl "0.2.13"]]
        :middleware [cider-nrepl.plugin/middleware
                     refactor-nrepl.plugin/middleware]
        :plugins [[cider/cider-nrepl "0.25.9"]
                  [refactor-nrepl "2.5.1"]
                  [clj-http "3.12.1"]
                  [lein-ancient "1.0.0-RC3"]
                  [lein-marginalia "0.9.1"]
                  [lein-shell "0.5.0"]
                  ]
        }
 }

