(ns scheduler.db
  (:require [next.jdbc :as jdbc]
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
       id            INTEGER  PRIMARY KEY,
       create_time   DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
     );
     CREATE TABLE agenda (
       test_id      INTEGER  NOT NULL,
       timestamp    DATETIME NOT NULL,
       command      JSON     NOT NULL,
       component_id TEXT     NOT NULL,

       PRIMARY KEY(test_id, timestamp),
       FOREIGN KEY(test_id) REFERENCES test(id)
     );"]))

;; TODO(stevan): disable foreign key check first?
(defn destroy-db!
  []
  (jdbc/execute! ds ["DROP TABLE test; DROP TABLE agenda;"]))

;; TODO(stevan): This should be done by the generator later...
(defn create-test!
  [])

(defn load-test!
  [])
