(ns scheduler.handler-test
  (:require [scheduler.handler :as sut]
            [clojure.test :as t]))

(t/deftest route-test
  (t/is (= {:status 200
            :body {:total 3}} (sut/app {:request-method :get
                                        :uri "/api/math"
                                        :query-params {:x "1", :y "2"}}))))
