{:user
 {:dependencies [[cljfmt  "0.5.3"]]
  :plugins [[lein-ancient "0.6.10"]
            [lein-cljfmt "0.5.3"]
            [lein-try  "0.4.3"]
            [lein-kibit "0.1.2"]
            [lein-pprint  "1.1.1"]
            [jonase/eastwood  "0.2.3"]
            [cider/cider-nrepl "0.12.0"]
            [refactor-nrepl "2.2.0"]
            [venantius/ultra "0.4.1"]]
  :cljfmt {:indents {defrecord [[:block 1]]}}}}
