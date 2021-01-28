(ns scheduler.db
  (:require [clojure.java.shell :as shell]
            [scheduler.spec :refer [>defn =>]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [scheduler.json :as json]
            [scheduler.time :as time]))

(set! *warn-on-reflection* true)

(def db nil)
(def ds nil)

(defn setup-db
  [db-file]
  (alter-var-root #'db
                  (constantly {:dbtype "sqlite" :dbname db-file}))
  (alter-var-root #'ds
                  (constantly (jdbc/get-datasource db))))

(defn read-db!
  "Import an SQLite database dump."
  [db-file dump-file]
  (shell/sh "sqlite3" db-file :in (str ".read " dump-file)))

(defn load-test!
  [test-id]
  (->> (jdbc/execute!
        ds
        ["SELECT agenda FROM test_info WHERE test_id = ?" test-id]
        {:builder-fn rs/as-unqualified-lower-maps})
       first ;; we should only have one test for the test_id..
       :agenda
       json/read
       (mapv #(update % :at time/instant))))

(defn next-run-id!
  [test-id]
  (jdbc/execute-one!
   ds
   ["SELECT IFNULL(MAX(run_id), -1) + 1 as `run-id` FROM run_info WHERE test_id = ?" test-id]
   {:return-keys true :builder-fn rs/as-unqualified-lower-maps}))

(comment
  (setup-db "/tmp/test.sqlite3")
  (destroy-db!)
  (create-db!)
  (create-test!)
  (insert-agenda! 1 0 "invoke" "inc" "{\"id\": 1}" "client:0" "node1" "1970-01-01T00:00:00Z")
  (insert-agenda! 1 1 "invoke" "get" "{\"id\": 1}" "client:0" "node1" "1970-01-01T00:00:01Z")
  (load-test! 1)
  (create-run! 0 123)
  (append-history! 1 :invoke "a" "{\"id\": 1}" 0))

(defn append-event!
  [test-id run-id event data]
   (jdbc/execute-one!
    ds
    ["INSERT INTO event_log (event, meta, data) VALUES (?,?,?)"
     event
     (json/write {:component "scheduler"
                  :test-id test-id
                  :run-id run-id})
     (json/write data)]
    {:return-keys true :builder-fn rs/as-unqualified-lower-maps}))

(defn append-network-trace!
  [test-id run-id data]
  (append-event! test-id run-id "NetworkTrace" data))

(defn append-create-run-event!
  [test-id run-id data]
  (append-event! test-id run-id "CreateRun" data))
