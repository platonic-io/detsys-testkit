(ns scheduler.core
  (:require [ring.middleware.defaults :as rmd]
            [ring.middleware.reload :as rmr]
            [ring.adapter.jetty :as jetty]
            [scheduler.handler :as handler]
            [scheduler.db :as db]
            [scheduler.pure :as pure])
  (:import [org.eclipse.jetty.server
            Server])
  (:gen-class))

(set! *warn-on-reflection* true)

(defonce server (atom nil))

(defn- start-server
  ([]
   (start-server false 3000))
  ([reload port]
   (db/setup-db "/tmp/test.sqlite3")
   (try (db/destroy-db!)
        (catch org.sqlite.SQLiteException e
          (println e)))
   (db/create-db!)
   (db/create-test!)
   (db/insert-agenda! 1 0 "write" "{\"value\": 1}" "client:0" "frontend" "1970-01-01T00:00:00Z")
   (db/insert-agenda! 1 1 "read" "{}" "client:0" "frontend" "1970-01-01T00:00:10Z")
   (reset! handler/data (pure/init-data))
   (reset! server (jetty/run-jetty (cond-> #'handler/app
                                     true (rmd/wrap-defaults rmd/api-defaults)
                                     ;; NOTE: wrap-reload needs to be disabled
                                     ;; when native-image is compiled and run.
                                     reload rmr/wrap-reload)
                                   {:port port
                                    :join? false}))))

(defn- stop-server
  []
  (when @server (.stop ^Server @server))
  (reset! server nil))

(defn- restart-server
  []
  (stop-server)
  (start-server))

(defn -main
  [& _args]
  (let [port (Integer/parseInt (or (System/getenv "SCHEDULER_PORT") "3000"))]
    (start-server false port)
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))

(comment
  (start-server true 3000)
  (stop-server)
  (restart-server) )
