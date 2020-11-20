-- +migrate Up
CREATE TABLE IF NOT EXISTS time_mapping (
  test_id      INTEGER  NOT NULL,
  run_id       INTEGER  NOT NULL,
  logical_time INTEGER  NOT NULL,
  simulated_time DATETIME NOT NULL,
  PRIMARY KEY(test_id, run_id, logical_time),
  FOREIGN KEY(test_id) REFERENCES test(id),
  FOREIGN KEY(run_id)  REFERENCES run(id));

-- +migrate Down
DROP TABLE IF EXISTS time_mapping;
