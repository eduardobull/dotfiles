{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-try  "0.4.3"]
                  [lein-kibit "0.1.5"]
                  [lein-pprint  "1.1.2"]
                  [jonase/eastwood  "0.2.4"]
                  [venantius/ultra "0.5.1"]
                  [com.palletops/lein-shorthand "0.4.0"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                       [alembic "0.3.2"]]
        :shorthand {. {pprint clojure.pprint/pprint
                       doc clojure.repl/doc
                       source clojure.repl/source
                       load-project alembic.still/load-project
                       add-dep alembic.still/distill}}}
 :repl {:plugins [[refactor-nrepl "2.3.1"]
                  [cider/cider-nrepl "0.14.0"]]}}
