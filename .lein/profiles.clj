{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-try  "0.4.3"]
                  [lein-kibit "0.1.5"]
                  [lein-pprint  "1.1.2"]
                  [jonase/eastwood  "0.2.4"]
                  [venantius/ultra "0.5.1"]
                  [com.palletops/lein-shorthand "0.4.0"]]

        :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                       [org.clojure/tools.trace "0.7.9"]
                       [org.clojure/tools.namespace "0.2.11"]
                       [pjstadig/humane-test-output "0.8.2"]
                       [alembic "0.3.2"]]

        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]

        :shorthand {. [clojure.repl/doc
                       clojure.repl/source
                       clojure.repl/pst
                       clojure.pprint/pprint
                       clojure.tools.namespace.repl/refresh
                       clojure.tools.trace/trace
                       clojure.tools.trace/trace-forms
                       clojure.tools.trace/trace-ns
                       clojure.tools.trace/untrace-ns
                       clojure.tools.trace/trace-vars
                       clojure.tools.trace/untrace-vars
                       alembic.still/load-project
                       alembic.still/distill
                       alembic.still/lein]}}

 :repl {:plugins [[refactor-nrepl "2.3.1"]
                  [cider/cider-nrepl "0.15.0"]]}}
