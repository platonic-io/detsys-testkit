(ns executor-test.handler
  (:require [reitit.ring :as ring]
            [reitit.ring.coercion :as rrc]
            [reitit.coercion.spec]
            [taoensso.timbre :as log]
            [jsonista.core :as j]
            [executor-test.pure :as pure]))

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

(defn command-handler
  [req]
  (let [{output :output, data' :data} (pure/command-executor @data (params req))]
    (reset! data data')
    (log/debug data')
    {:status 200
     :body (j/write-value-as-string output)}))

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
     ["/command" {:post {:responses {200 {:body ::data}}
                         :handler command-handler}}]
     ["/status" {:get {:response {200 {:body ::data}}
                       :handler status-handler}}]
     ["/debug" {:get {:response {200 {:body ::data}}
                      :handler debug-handler}}]
     ])))
