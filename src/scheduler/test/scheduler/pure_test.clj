(ns scheduler.pure-test
  (:require [scheduler.pure :as sut]
            [scheduler.json :as json]
            [clj-http.fake :as fake]
            [clojure.test :as t]
            [scheduler.db :as db]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s])
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
             {"http://localhost:3001/api/v1/event"
              (fn [_request] {:status 200
                              :headers {}
                              :body (json/write {:events []})})}
             (-> (sut/init-data)
                 (sut/load-test! {:test-id 1})
                 first
                 (sut/register-executor
                  {:executor-id "http://localhost:3001/api/v1/event"
                   :components ["component0"]})
                 first
                 (sut/create-run! {:test-id 1})
                 first
                 sut/run!
                 second)))))

(t/deftest run-with-events-test
  (let [fake-step! (fn [data]
                     (fake/with-fake-routes
                       {"http://localhost:3001/api/v1/event"
                        (fn [_request] {:status 200
                                        :headers {}
                                        :body
                                        (json/write
                                         {:events
                                          [{:kind "invoke"
                                            :event "a"
                                            :args {}
                                            :from "component0"
                                            :to "component0"}]})})}
                       (sut/step! data)))
        fake-run! (fn [data]
                    (fake/with-fake-routes
                      {"http://localhost:3001/api/v1/event"
                       (fn [_request] {:status 200
                                       :headers {}
                                       :body (json/write {:events []})})}
                      (sut/run! data)))]

    (t/is (= {:steps 2} ;; 3 in total, but `run!` only counts the ones it steps.
             (-> (sut/init-data)
                 (sut/load-test! {:test-id 1})
                 first
                 (sut/register-executor
                  {:executor-id "http://localhost:3001/api/v1/event"
                   :components ["component0"]})
                 first
                 (sut/create-run! {:test-id 1})
                 first
                 fake-step!
                 first
                 fake-run!
                 second)))))

(t/deftest executor-contract-register
  (let [from-executor (str "{\"executor-id\": \"http://localhost:3001/api/v1/event\","
                           "\"components\": [\"c1\", \"c2\"]}")]
    (t/is (->> from-executor
               json/read
               (s/valid? (s/keys :req-un [:scheduler.pure/executor-id
                                          :scheduler.pure/components]))))))

(t/deftest executor-contract-events
  (let [from-executor (str "{\"events\":"
                           "  [{\"from\": \"node\","
                           "    \"to\":   \"client:0\","
                           "    \"kind\": \"ok\","
                           "    \"event\": \"write\","
                           "    \"args\": {\"value\": 1}}"
                           "]}")
        edn (json/read from-executor)
        spec (s/keys :req-un [:scheduler.pure/events])]
    (t/is (s/valid? spec edn)
          (s/explain-str spec edn))))
