(ns scheduler.handler
  (:require [taoensso.timbre :as log]
            [scheduler.db :as db]
            [scheduler.json :as json]
            [scheduler.pure :as pure]
            [clojure.spec.alpha :as s]))

(set! *warn-on-reflection* true)

(def data (atom (pure/init-data)))

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
                (json/read))]
    (log/debug :body edn)
    (if (s/valid? command? edn)
      (let [_ (db/append-event! (:test-id data)
                                (:run-id data)
                                (:command edn)
                                "Start"
                                (:parameters edn))
            parameters (:parameters edn)
            [data' output] (call (:command edn) (if (empty? parameters)
                                                  [@data]
                                                  [@data parameters]))]
        (db/append-event! (:test-id data)
                          (:run-id data)
                          (:command edn)
                          "End"
                          (:parameters edn))
        (if (pure/error-state? (:state data'))
          {:status 400
           :headers {"Content-Type" "application/json; charset=utf-8"}
           :body (json/write {:error (:state data')})}
          (do
            (reset! data data')
            {:status 200
             :headers {"Content-Type" "application/json; charset=utf-8"}
             :body (json/write output)})))
      {:status 400
       :headers {"Content-Type" "text/plain; charset=utf-8"}
       ;; TODO(stevan): can we return readable json instead of plain text?
       ;; :body (json/write-str {:error (s/explain-str command? edn)})})))
       :body (s/explain-str command? edn)})))

(def app handler)
