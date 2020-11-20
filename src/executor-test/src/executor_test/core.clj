(ns executor-test.core
  (:require
   [ring.middleware.defaults :as rmd]
   [ring.middleware.reload :as rmr]
   [org.httpkit.server :as server]
   [executor-test.handler :as handler]))

(defn run-server
  [port]
  (server/run-server (-> #'handler/app
                         (rmd/wrap-defaults rmd/api-defaults)
                         rmr/wrap-reload)
                     {:port port}))

(defn -main
  [& _args]
  (let [port 3001]
  (run-server port)
  (println (str "Running webserver at http://127.0.0.1:" port "/"))))
