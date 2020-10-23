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
        ["SELECT * FROM agenda WHERE test_id = ? ORDER BY id ASC" test-id]
        {:return-keys true :builder-fn rs/as-unqualified-lower-maps})
       (mapv #(-> %
                  (dissoc :id :test_id)
                  (update :at time/instant)
                  (update :args json/read)))))

(defn create-run!
  [test-id seed]
  (jdbc/execute-one!
   ds
   ["INSERT INTO run (test_id, id, seed)
     VALUES (?, (SELECT IFNULL(MAX(id), -1) + 1 FROM run WHERE test_id = ?), ?)"
    test-id test-id seed])
  (jdbc/execute-one!
   ds
   ["SELECT MAX(id) as `run-id` FROM run WHERE test_id = ?" test-id]
   {:return-keys true :builder-fn rs/as-unqualified-lower-maps}))

(defn append-history!
  [test-id run-id kind event args process]
  (jdbc/execute-one!
   ds
   ["INSERT INTO history (test_id, run_id, id, kind, event, args, process)
     VALUES (?, ?, (SELECT IFNULL(MAX(id), -1) + 1 FROM history WHERE run_id = ?), ?, ?, ?, ?)"
    test-id run-id run-id (name kind) event args process]
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
  (append-history! 1 :invoke "a" "{\"id\": 1}" 0)
  )

(defn append-trace!
  [test-id run-id message args from to at]
  (jdbc/execute-one!
   ds
   ["INSERT INTO network_trace (test_id, run_id, id, message, args, `from`, `to`, at)
     VALUES (?, ?, (SELECT IFNULL(MAX(id), -1) + 1 FROM network_trace WHERE run_id = ?), ?, ?, ?, ?, ?)"
    test-id run-id run-id message args from to at]
   {:return-keys true :builder-fn rs/as-unqualified-lower-maps}))
