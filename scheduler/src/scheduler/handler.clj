(ns scheduler.handler
  (:require [reitit.ring :as ring]
            [reitit.ring.coercion :as rrc]
            [reitit.coercion.spec]
            [taoensso.timbre :as log]
            [jsonista.core :as j]
            [scheduler.pure :as pure]))

(def data (atom (pure/init-data)))

(def mapper
  (j/object-mapper
   {:encode-key-fn keyword
    :decode-key-fn true}))

(defn params
  [req]
  (-> req
      :body
      (j/read-value mapper)))

(defn register-handler
  [req]
  (let [{output :output, data' :data} (pure/register-executor @data (params req))]
    (reset! data data')
    ;; (log/debug (params req))
    (log/debug data')
    {:status 200
     :body (j/write-value-as-string {:remaining-executors output})}))

(defn status-handler
  [_req]
  {:status 200
   :body (j/write-value-as-string @data)})

(defn debug-handler
  [req]
  {:status 200
   :body (-> req params str)})

;; (j/read-value "{\"executor-id\": \"executor1\", \"components\": [\"a\", \"b\"]}" mapper)

(def app
  (ring/ring-handler
   (ring/router
    ["/api"
     ["/register" {:put {:responses {200 {:body {:remaining-executors pos-int?}}}
                         :handler register-handler}}]
     ["/status" {:get {:response {200 {:body ::data}}
                       :handler status-handler}}]
     ["/debug" {:get {:response {200 {:body ::data}}
                      :handler debug-handler}}]
     ])))
    ;; {:data {:coercion reitit.coercion.spec/coercion
    ;;         :middleware [rrc/coerce-exceptions-middleware
    ;;                      rrc/coerce-request-middleware
    ;;                      rrc/coerce-response-middleware]}})))
