(ns scheduler.core
  (:require [ring.middleware.defaults :as rmd]
            [ring.middleware.reload :as rmr]
            [org.httpkit.server :as server]
            [scheduler.handler :as handler])
  (:gen-class))

(set! *warn-on-reflection* true)

(defonce server (atom nil))

(defn- start-server
  ([]
   (start-server 3000))
  ([port]
   (reset! server (server/run-server (-> #'handler/app
                                         (rmd/wrap-defaults rmd/api-defaults)
                                         rmr/wrap-reload)
                                     {:port port}))))

(defn- stop-server
  []
  (when @server (@server))
  (reset! server nil))

(defn- restart-server
  []
  (stop-server)
  (start-server))

(defn -main
  [& _args]
  (let [port (Integer/parseInt (or (System/getenv "SCHEDULER_PORT") "3000"))]
    (start-server port)
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))

(comment
  (start-server)
  (stop-server))
