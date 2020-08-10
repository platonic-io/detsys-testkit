(ns scheduler.core
  (:require [ring.middleware.defaults :as rmd]
            [ring.middleware.reload :as rmr]
            [org.httpkit.server :as server]
            [scheduler.handler :as handler])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn run-server
  [port]
  (server/run-server (-> #'handler/app
                         (rmd/wrap-defaults rmd/api-defaults)
                         rmr/wrap-reload)
                     {:port port}))

(defn -main
  [& _args]
  (let [port (Integer/parseInt (or (System/getenv "SCHEDULER_PORT") "3000"))]
    (run-server port)
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))

(comment
(defonce server (run-server 3000))
(server)
)
