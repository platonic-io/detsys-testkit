(ns scheduler.handler-test
  (:require [scheduler.handler :as sut]
            [scheduler.pure :as pure]
            [clojure.test :as t]
            [jsonista.core :as j]))

(defn fixture
  [f]
  (reset! sut/data (pure/init-data))
  (f))

(t/use-fixtures :once fixture)

(t/deftest route-test
  (t/is (= {:status 200
            :body (j/write-value-as-string (pure/init-data))}
           (sut/app {:request-method :get
                     :uri "/api/status"})))
  (t/is (= {:status 200
            :body (j/write-value-as-string {:remaining-executors 0})}
           (sut/app {:request-method :put
                     :uri "/api/register"
                     :body (j/write-value-as-string
                            {:executor-id "executor1"
                             :components ["component1" "component2"]})}))))
