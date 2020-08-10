(ns scheduler.core
  (:require [ring.middleware.defaults :as rmd]
            [org.httpkit.server :as server]
            [scheduler.handler :as handler])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main
  [& _args]
  (let [port (Integer/parseInt (or (System/getenv "SCHEDULER_PORT") "3000"))]
    (server/run-server (rmd/wrap-defaults #'handler/app rmd/api-defaults) {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))
