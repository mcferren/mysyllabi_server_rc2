;;; Benjamin McFerren
;;; DePaul Univeristy CDM
;;; CSC 358
;;; 3/19/2014

(defproject helloworld "1.0.0-SNAPSHOT"
  :description "csc358 final project"
  :url "http://localhost:9000/"
  :license {:name "FIXME: choose"
            :url "http://example.com/FIXME"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [compojure "1.1.8"]
                 [ring/ring-json "0.3.1"]
                 [ring/ring-core "1.4.0"]
                 [ring-router "0.2-SNAPSHOT"]
                 [ring/ring-jetty-adapter "1.3.2"]
                 [jumblerg/ring.middleware.cors "1.0.1"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [environ "0.4.0"]
                 [hiccup "1.0.0"]
                 [com.novemberain/monger "3.0.0"]
                 [clojurewerkz/neocons "3.1.0-rc1"]
                 [org.clojure/data.json "0.2.5"]
                 [clj-wamp "1.0.2"]
                 [clj-http "2.0.0"]
                 [clojure.joda-time "0.6.0"]
                 [com.cemerick/friend "0.2.1"]
                 [org.marianoguerra/friend-json-workflow "0.2.1"]
                 [buddy/buddy-hashers "0.4.0"]
                 [buddy/buddy-auth "0.4.0"]
                 [crypto-random "1.2.0"]
                 [bouncer "0.3.3"]]
  :min-lein-version "2.0.0"
  :plugins [[environ/environ.lein "0.2.1"]
            [lein-ring "0.7.3"]]
  :hooks [environ.leiningen.hooks]
  :profiles {:dev {:dependencies [[ring-mock "0.1.3"]]}}
  :ring {:handler recursiftion.controller/app})
