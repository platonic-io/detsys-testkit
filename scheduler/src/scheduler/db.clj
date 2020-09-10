(ns scheduler.db
  (:require [clojure.java.shell :as shell]
            [scheduler.spec :refer [>defn =>]]
            [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [next.jdbc.result-set :as rs]
            [scheduler.json :as json]))

(set! *warn-on-reflection* true)

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
       id           INTEGER  NOT NULL,
       command      JSON     NOT NULL,
       `from`       TEXT     NOT NULL,
       `to`         TEXT     NOT NULL,
       at           INTEGER  NOT NULL,
       PRIMARY KEY(test_id, id),
       FOREIGN KEY(test_id) REFERENCES test(id))"])
  (jdbc/execute! ds ["
     CREATE TABLE run (
       test_id       INTEGER  NOT NULL,
       id            INTEGER  NOT NULL,
       seed          INTEGER  NOT NULL,
       create_time   DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
       PRIMARY KEY(test_id, id),
       FOREIGN KEY(test_id) REFERENCES test(id))"])
  (jdbc/execute! ds ["
     CREATE TABLE history (
       run_id       INTEGER  NOT NULL,
       id           INTEGER  NOT NULL,
       command      JSON     NOT NULL,
       `from`       TEXT     NOT NULL,
       `to`         TEXT     NOT NULL,
       at           INTEGER  NOT NULL,
       PRIMARY KEY(run_id, id),
       FOREIGN KEY(run_id) REFERENCES run(id))"])
  )

(defn destroy-db!
  []
  (jdbc/execute! ds ["DROP TABLE test"])
  (jdbc/execute! ds ["DROP TABLE agenda"])
  (jdbc/execute! ds ["DROP TABLE run"])
  (jdbc/execute! ds ["DROP TABLE history"]))

(defn read-db!
  "Import an SQLite database dump."
  [db-file dump-file]
  (shell/sh "sqlite3" db-file :in (str ".read " dump-file)))

;; TODO(stevan): This should be done by the generator later...
(>defn create-test!
  []
  [=> nat-int?]
  (-> (jdbc/execute-one! ds ["INSERT INTO test DEFAULT VALUES"]
                         {:return-keys true :builder-fn rs/as-unqualified-lower-maps})
      vals
      first))

(defn insert-agenda!
  [test-id id command from to at]
  (jdbc/execute-one!
   ds
   ["INSERT INTO agenda (test_id, id, command, `from`, `to`, at)
     VALUES (?, ?, ?, ?, ?, ?)" test-id id command from to at]
   {:return-keys true :builder-fn rs/as-unqualified-lower-maps}))

(defn load-test!
  [test-id]
  (->> (jdbc/execute!
        ds
        ["SELECT * FROM agenda WHERE test_id = ? ORDER BY id ASC" test-id]
        {:return-keys true :builder-fn rs/as-unqualified-lower-maps})
       (mapv #(-> %
                  (dissoc :id :test_id)
                  (update :command json/read)))))

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
  [run-id command from to at]
  (jdbc/execute-one!
   ds
   ["INSERT INTO history (run_id, id, command, `from`, `to`, at)
     VALUES (?, (SELECT IFNULL(MAX(id), -1) + 1 FROM history WHERE run_id = ?), ?, ?, ?, ?)"
    run-id run-id command from to at]
   {:return-keys true :builder-fn rs/as-unqualified-lower-maps}))

(comment
  (setup-db "/tmp/test.sqlite3")
  (destroy-db!)
  (create-db!)
  (insert-agenda! 1 0 "{\"name\": \"a\", \"parameters\": []}"
                  "client0" "component0" 0)
  (insert-agenda! 1 1 "{\"name\": \"b\", \"parameters\": []}"
                  "client0" "component0" 1)
  (load-test! 1)
  (create-run! 0 123)
  (append-history! 1 "{\"name\": \"a\", \"parameters\": []}" "client0" "component0" 0)
  (append-history! 1 "{\"name\": \"b\", \"parameters\": []}" "client0" "component0" 1)
  (append-history! 2 "{\"name\": \"a\", \"parameters\": []}" "client0" "component0" 0)
  (append-history! 2 "{\"name\": \"b\", \"parameters\": []}" "client0" "component0" 1)
  )
