(defproject rawsugar "master.d124ac8f"
  :repositories 
  [["github" {:url "https://maven.pkg.github.com/ParkerICI/mvn-packages"
              :sign-releases false
              :username :env/github_user
              :password :env/github_password}]]
  :dependencies [;; server side
                 [org.clojure/clojure "1.10.3"] ;back off from 1.11, update-vals conflic
                 [org.clojure/data.csv "0.1.4"]
                 [org.clojure/tools.cli "1.0.206"]
                 [aero "1.1.6"]
                 [com.datomic/client-pro "0.8.28" ; "0.9.63" gets weird erro ;   "1.0.75"
                  :exclusions [org.eclipse.jetty/jetty-client
                               org.eclipse.jetty/jetty-http
                               org.eclipse.jetty/jetty-util]]
                 [org.eclipse.jetty/jetty-server "9.4.12.v20180830"]
                 [org.eclipse.jetty/jetty-client "9.4.12.v20180830"]
                 [org.eclipse.jetty/jetty-http "9.4.12.v20180830"]
                 [org.eclipse.jetty/jetty-util "9.4.12.v20180830"]

                 [org.slf4j/slf4j-simple "1.7.29"]                   ;required to turn off warning
                 [org.candelbio/multitool "0.1.0"]
                 [org.candelbio/alzabo "1.0.0" :exclusions [re-frame]]
                 [com.taoensso/timbre "4.10.0"]
                 [compojure "1.6.1" :exclusions [ring.core ring.codec]]
                 [ring "1.8.0"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-jetty-adapter "1.7.1"]
                 [ring-logger "1.0.1"]
                 [ring-oauth2 "0.1.4"]
                 [ring-middleware-format "0.7.4" :exclusions [javax.xml.bind/jaxb-api]]
                 [bk/ring-gzip "0.3.0"]
                 [dk.ative/docjure "1.13.0"]
                 [com.google.cloud/google-cloud-storage "1.75.0"
                  :exclusions [com.google.errorprone/error_prone_annotations
                               com.google.protobuf/protobuf-java
                               com.google.guava/guava
                               com.fasterxml.jackson.core/jackson-core]]
                 [com.google.auth/google-auth-library-oauth2-http "0.18.0"
                  :exclusions [com.google.guava/guava]]
                 [com.nimbusds/nimbus-jose-jwt "8.2.1"]
                 [me.raynes/fs "1.4.6"]
                 [org.apache.tika/tika-core "1.20"]
                 [environ "1.1.0"]
                 [trptcolin/versioneer "0.2.0"]
                 [com.cemerick/url "0.1.1"]
                 [metasoarous/oz "1.6.0-alpha6"  ; warning higher versions have dep problems
;                 [metasoarous/oz "1.6.0-alpha36"
;                 [metasoarous/oz "2.0.0-alpha5"
                  :exclusions [com.google.errorprone/error_prone_annotations
                               com.taoensso/encore]]
                 ;; Necessary to run in Java > 8, see https://stackoverflow.com/questions/43574426/how-to-resolve-java-lang-noclassdeffounderror-javax-xml-bind-jaxbexception-in-j
                 [javax.xml.bind/jaxb-api "2.2.11"]
                 [com.sun.xml.bind/jaxb-core "2.2.11"]
                 [com.sun.xml.bind/jaxb-impl "2.2.11"]
                 [javax.activation/activation "1.1.1"]

                 ;; client side
                 [org.clojure/clojurescript "1.11.60"]
                 [com.google.javascript/closure-compiler-unshaded "v20220803"]
                 [reagent "0.10.0"]
                 [re-frame "0.12.0"]
                 [re-com "2.13.2"]
;;                  [cljsjs/react-select "2.4.4-0"]
                 [com.cemerick/url "0.1.1"]
                 [com.andrewmcveigh/cljs-time "0.5.2"]
                 [inflections "0.13.2"]
                 [cljs-ajax "0.8.0"]
                 [bidi "2.1.6"]         ;routing
                 [kibu/pushy "0.3.8"]
                 [oauthentic "1.0.1"]
                 [slingshot "0.12.2"]
                 [thheller/shadow-cljs "2.20.10"] ;TODO maybe only in dev profile
                 ]


  ;; Trying to turn off spurious log message. Doesn't work – flags are set but no effect
  :jvm-opts ["-Dorg.eclipse.jetty.util.log.class=org.eclipse.jetty.util.log.StdErrLog"
             "-Dorg.eclipse.jetty.LEVEL=OFF"
             "-Xmx3G"]

  :plugins [[lein-ring "0.12.5"]
            [lein-set-version "0.4.1"]
            [lein-environ "1.1.0"]
            [lein-test-out "0.3.1"]
            [test2junit "1.1.2"]]

  :test2junit-output-dir "target/test2junit"

  :min-lein-version "2.5.3"

  :main ^:skip-aot org.candelbio.rawsugar.cli
  :ring {:handler org.candelbio.rawsugar.handler/app}

  :source-paths ["src/cljc" "src/clj" "src/cljs"] 
  :test-paths ["test/clj"]

  :resource-paths ["resources"] 

  :jar-name "rawsugar.jar"
  :uberjar-name "rawsugar-standalone.jar"

  :target-dir "target"

  :clean-targets ^{:protect false} ["target" "resources/public/cljs-out"]

  ;; TODO not all of these are used. Should correspond to tiers I think.
  :profiles
  {:test
   {:dependencies [[mock-clj "0.2.1"]
                   [clj-http "3.9.1"]
                   ]
    :env {:tier "test"
          :gcs-bucket "pici-rawsugar-dev"
          :gcs-project "pici-dev"
          :autorespond? "y"
          :datomic-endpoint "localhost:8999"
          :datomic-db-name "rawsugar" 
          }
    }
   :dev
   {:dependencies [[mock-clj "0.2.1"]
                   [clj-http "3.9.1"]
                   ;; Doesn't work, has some kind of dependency conflict?
                   ;; [com.datomic/dev-local "0.9.232"]         ; See https://cognitect.com/dev-tools and https://docs.datomic.com/cloud/dev-local.html
                   [cider/piggieback "0.5.0"] ; not needed for cider-jack-in-cljs
                   [com.bhauman/rebel-readline-cljs "0.1.4"] ;TODO explore this
                   [day8.re-frame/re-frame-10x "1.5.0"]]
    :plugins      []
    }
   :uberjar
   {
    ;; TODO don't want the following here but can't build without it? I'm baffled.
    :dependencies [[day8.re-frame/re-frame-10x "1.5.0"]
                   ]
    ;; :prep-tasks ["fig:build-dev" "fig:build-prod" "compile"]
    ;; defining the production env here does not work due to environ limitations. THe configs have
    ;; to be passed as shell vars, hance the rawsugar bash script.
    :omit-source true
    :aot :all                           ;excessive but catches bugs early
    }
   :prod
   {:dependencies []
    }
   }
  )
