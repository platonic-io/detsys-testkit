-- +migrate Up

CREATE TABLE IF NOT EXISTS faults (
  test_id      INTEGER  NOT NULL,
  run_id       INTEGER  NOT NULL,
  faults       JSON     NOT NULL,
  version      TEXT     NOT NULL,
  statistics   TEXT     NOT NULL,
  PRIMARY KEY(test_id, run_id),
  FOREIGN KEY(test_id) REFERENCES test(id),
  FOREIGN KEY(run_id)  REFERENCES run(id));

-- +migrate Down

DROP TABLE IF EXISTS faults;
