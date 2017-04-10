{:user {:plugins [[lein-ancient "0.6.10"]
                  [lein-try  "0.4.3"]
                  [lein-kibit "0.1.3"]
                  [lein-pprint  "1.1.2"]
                  [jonase/eastwood  "0.2.3"]
                  [venantius/ultra "0.5.1"]
                  [pjstadig/humane-test-output "0.8.1"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]}
 :repl {:plugins [[refactor-nrepl "2.3.0"]
                  [cider/cider-nrepl "0.14.0"]]}}
