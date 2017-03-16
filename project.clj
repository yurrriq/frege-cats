(defproject frege-cats "0.1.0-SNAPSHOT"
  :description "Cats bindings for Frege data types"
  :url "https://github.com/yurrriq/frege-cats"
  :license {:name "MIT"
            :url "https://yurrriq.mit-license.org"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.frege-lang/frege "3.24.100.1" :classifier "jdk8"]
                 [funcool/cats "2.0.0"]]
  ;; :plugins [[org.clojars.yurrriq/lein-fregec "3.24.100.1-SNAPSHOT"]]

  :source-paths ["src/clojure"]
  ;; :frege-source-paths ["src/frege"]

  :profiles {:uberjar {:aot :all
                       ;; :prep-tasks ["fregec" "compile"]
                       }})
