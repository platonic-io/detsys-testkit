(ns scheduler.handler-test
  (:require [scheduler.handler :as sut]
            [scheduler.pure :as pure]
            [scheduler.json :as json]
            [clojure.test :as t]))

(defn fixture
  [f]
  (reset! sut/data (pure/init-data))
  (f))

(t/use-fixtures :once fixture)

(t/deftest route-test
  (t/is (= {:status 200
            :headers {"Content-Type" "application/json; charset=utf-8"}
            :body (json/write (pure/init-data))}
           (sut/app {:request-method :post
                     :uri "/"
                     :body (json/write
                            {:command "status"
                             :parameters {}})}) ))
  (t/is (= {:status 200
            :headers {"Content-Type" "application/json; charset=utf-8"}
            :body (json/write {:remaining-executors 0})}
           (sut/app {:request-method :post
                     :uri "/"
                     :body (json/write
                            {:command "register-executor"
                             :parameters
                             {:executor-id "executor1"
                              :components ["component1" "component2"]}})}))))
