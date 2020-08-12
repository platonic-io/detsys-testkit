(ns scheduler.handler
  (:require [reitit.ring :as ring]
            [reitit.ring.coercion :as rrc]
            [reitit.coercion.spec]
            [taoensso.timbre :as log]
            [jsonista.core :as j]
            [scheduler.pure :as pure]
            [clojure.spec.alpha :as s]))

(def data (atom (pure/init-data)))

(def mapper
  (j/object-mapper
   {:encode-key-fn keyword
    :decode-key-fn true}))

(s/def ::command string?)
(s/def ::parameters map?)

(def command? (s/keys :req-un [::command ::parameters]))

(defn call [f args]
  (log/debug :call f args)
  (apply (ns-resolve 'scheduler.pure (symbol f)) args))

(defn handler
  [req]
  (let [edn (-> req
                :body
                (j/read-value mapper))]
    (if (s/valid? command? edn)
      (let [parameters (:parameters edn)
            [data' output] (call (:command edn) (if (empty? parameters)
                                                  [@data]
                                                  [@data parameters]))]
        (reset! data data')
        {:status 200
         :headers {"Content-Type" "application/json; charset=utf-8"}
         :body (j/write-value-as-string output)})
      {:status 400
       :headers {"Content-Type" "text/plain; charset=utf-8"}
       ;; :body (j/write-value-as-string {:error (s/explain-str command? edn)})})))
       :body (s/explain-str command? edn)})))

(def app
  (ring/ring-handler
   (ring/router
    ["/api"
     ["/command" {:post {:handler handler}}]])))
