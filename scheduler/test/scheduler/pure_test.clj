(ns scheduler.pure-test
  (:require [scheduler.pure :as sut]
            [scheduler.json :as json]
            [clj-http.fake :as fake]
            [clojure.test :as t]
            [scheduler.db :as db]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(defn fixture
  [f]
  (let [tmpfile (File/createTempFile "pure-test" ".sqlite3" (new File "/tmp"))]
    (db/read-db! (str tmpfile) "test/scheduler/sqlite3.dump")
    (db/setup-db (str tmpfile))
    (f)
    (io/delete-file tmpfile)))

(t/use-fixtures :once fixture)

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
                  {:executor-id "http://localhost:3001" :components ["component0"]})
                 first
                 sut/run!
                 second)))))

(t/deftest run-with-responses-test
  (let [fake-step! (fn [data]
                     (fake/with-fake-routes
                       {"http://localhost:3001/api/command"
                        (fn [_request] {:status 200
                                        :headers {}
                                        :body
                                        (json/write
                                         {:responses
                                          [{:command {:name "a", :parameters []}
                                            :from "component0"
                                            :to "component0"}]})})}
                       (sut/step! data)))
        fake-run! (fn [data]
                    (fake/with-fake-routes
                      {"http://localhost:3001/api/command"
                       (fn [_request] {:status 200
                                       :headers {}
                                       :body (json/write {:responses []})})}
                      (sut/run! data)))]

    (t/is (= {:steps 2} ;; 3 in total, but `run!` only counts the ones it steps.
             (-> (sut/init-data)
                 (sut/load-test! {:test-id 1})
                 first
                 (sut/register-executor
                  {:executor-id "http://localhost:3001" :components ["component0"]})
                 first
                 fake-step!
                 first
                 fake-run!
                 second)))))
