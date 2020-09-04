(ns scheduler.db
  (:require [scheduler.spec :refer [>defn =>]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]))

(def db nil)
(def ds nil)

(defn setup-db
  [db-file]
  (alter-var-root #'db
                  (constantly {:dbtype "sqlite" :dbname db-file}))
  (alter-var-root #'ds
                  (constantly (jdbc/get-datasource db))))

(defn create-db!
  []
  (jdbc/execute! ds ["
     CREATE TABLE test (
       id            INTEGER  PRIMARY KEY AUTOINCREMENT,
       create_time   DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP)"])
  (jdbc/execute! ds ["
     CREATE TABLE agenda (
       test_id      INTEGER  NOT NULL,
       ix           INTEGER  NOT NULL,
       command      JSON     NOT NULL,
       `from`       TEXT     NOT NULL,
       `to`         TEXT     NOT NULL,
       at           INTEGER  NOT NULL,

       PRIMARY KEY(test_id, ix),
       FOREIGN KEY(test_id) REFERENCES test(id))"]))

(defn destroy-db!
  []
  (jdbc/execute! ds ["DROP TABLE test"])
  (jdbc/execute! ds ["DROP TABLE agenda"]))

;; TODO(stevan): This should be done by the generator later...
(>defn create-test!
  []
  [=> nat-int?]
  (-> (jdbc/execute-one! ds ["INSERT INTO test DEFAULT VALUES"]
                         {:return-keys true :builder-fn rs/as-unqualified-lower-maps})
      vals
      first))

(defn insert-agenda!
  [test-id ix command from to at]
  (-> (jdbc/execute-one!
       ds
       ["INSERT INTO agenda (test_id, ix, command, `from`, `to`, at)
         VALUES (?, ?, ?, ?, ?, ?)" test-id ix command from to at]
       {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))

(defn load-test!
  [test-id]
  (-> (jdbc/execute!
       ds
       ["SELECT * FROM  agenda WHERE test_id = ? ORDER BY ix ASC" test-id]
       {:return-keys true :builder-fn rs/as-unqualified-lower-maps})))

(comment
  (setup-db "/tmp/test.sqlite3")
  (destroy-db!)
  (create-db!)
  (insert-agenda! 1 0 "{\"name\": \"a\", \"parameters\": []}"
                  "client0" "component0" 0)
  (insert-agenda! 1 1 "{\"name\": \"b\", \"parameters\": []}"
                  "component0" "clinet0" 1)
  (load-test! 1)
  )
