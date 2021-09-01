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
        :plugins [[cider/cider-nrepl "0.26.0"]
                  [refactor-nrepl "3.0.0-pr301"]
                  [clj-http "3.12.3"]
                  [lein-ancient "1.0.0-RC3"]
                  [lein-marginalia "0.9.1"]
                  [lein-shell "0.5.0"]
                  ]
        }
 }

