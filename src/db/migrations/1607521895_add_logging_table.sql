-- +migrate Up
CREATE TABLE IF NOT EXISTS log_trace (
  test_id      INTEGER  NOT NULL,
  run_id       INTEGER  NOT NULL,
  id           INTEGER  NOT NULL,
  component    TEXT     NOT NULL,
  log          TEXT     NOT NULL,
  simulated_time DATETIME NOT NULL,
  PRIMARY KEY(test_id, run_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id),
  FOREIGN KEY(run_id)  REFERENCES run(id));

-- +migrate Down
DROP TABLE IF EXISTS log_trace;
