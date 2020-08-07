(ns scheduler.handler
  (:require [reitit.ring :as ring]
            [reitit.ring.coercion :as rrc]
            [reitit.coercion.spec]))

(def app
  (ring/ring-handler
   (ring/router
    ["/api"
     ["/math" {:get {:parameters {:query {:x int?, :y int?}}
                     :responses {200 {:body {:total pos-int?}}}
                     :handler (fn [{{{:keys [x y]} :query} :parameters}]
                                {:status 200
                                 :body {:total (+ x y)}})}}]]
    {:data {:coercion reitit.coercion.spec/coercion
            :middleware [rrc/coerce-exceptions-middleware
                         rrc/coerce-request-middleware
                         rrc/coerce-response-middleware]}})))
