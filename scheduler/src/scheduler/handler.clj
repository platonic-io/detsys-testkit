(ns scheduler.handler
  (:require [clojure.tools.logging :as log]
            [clojure.data.json :as json]
            [scheduler.pure :as pure]
            [clojure.spec.alpha :as s]))

(def data (atom (pure/init-data)))

(s/def ::command string?)
(s/def ::parameters map?)

(def command? (s/keys :req-un [::command ::parameters]))

(defn call [f args]
  (log/debug :call f args)
  (apply (ns-resolve 'scheduler.pure (symbol f)) args))

(defn error-state?
  [state]
  (some? (re-matches #"^error-.*$" (name state))))

(defn handler
  [req]
  (let [edn (-> req
                :body
                (json/read-str :key-fn keyword))]
    (if (s/valid? command? edn)
      (let [parameters (:parameters edn)
            [data' output] (call (:command edn) (if (empty? parameters)
                                                  [@data]
                                                  [@data parameters]))]
        (if (error-state? (:state data'))
          {:status 400
           :headers {"Content-Type" "application/json; charset=utf-8"}
           :body (json/write-str {:error (:state data')})}
          (do
            (reset! data data')
            {:status 200
             :headers {"Content-Type" "application/json; charset=utf-8"}
             :body (json/write-str output)})))
      {:status 400
       :headers {"Content-Type" "text/plain; charset=utf-8"}
       ;; TODO(stevan): can we return readable json instead of plain text?
       ;; :body (json/write-str {:error (s/explain-str command? edn)})})))
       :body (s/explain-str command? edn)})))

(def app handler)
