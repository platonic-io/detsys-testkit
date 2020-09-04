(ns scheduler.pure-test
  (:require [scheduler.pure :as sut]
            [scheduler.json :as json]
            [clj-http.fake :as fake]
            [clojure.test :as t]))

(t/deftest run-test
  (t/is (= {:steps 2}
           (fake/with-fake-routes
             {"http://localhost:3001/api/command"
              (fn [_request] {:status 200
                              :headers {}
                              :body (json/write {:responses []})})}
             (-> (sut/init-data)
                 (sut/load-test! {:test-id 1})
                 first
                 (sut/register-executor
                  {:executor-id "http://localhost:3001" :components ["c"]})
                 first
                 sut/run!
                 second)))))
