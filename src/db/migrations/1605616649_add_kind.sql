-- +migrate Up

ALTER TABLE network_trace
  RENAME TO network_trace_old;

CREATE TABLE IF NOT EXISTS network_trace (
  test_id      INTEGER  NOT NULL,
  run_id       INTEGER  NOT NULL,
  id           INTEGER  NOT NULL,
  message      TEXT     NOT NULL,
  args         JSON     NOT NULL,
  `from`       TEXT     NOT NULL,
  `to`         TEXT     NOT NULL,
  kind         TEXT     NOT NULL,
  sent_logical_time INTEGER NOT NULL,
  at           INTEGER  NOT NULL,
  dropped      INT2     NOT NULL,
  PRIMARY KEY(test_id, run_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id),
  FOREIGN KEY(run_id)  REFERENCES run(id));

INSERT INTO network_trace (test_id, run_id, id, message, args, `from`, `to`, kind, sent_logical_time, at, dropped)
SELECT test_id, run_id, id, message, args, `from`, `to`, 'unknown', sent_logical_time, at, dropped
  FROM network_trace_old;

DROP TABLE IF EXISTS network_trace_old;

-- +migrate Down

CREATE TABLE IF NOT EXISTS network_trace_new (
  test_id      INTEGER  NOT NULL,
  run_id       INTEGER  NOT NULL,
  id           INTEGER  NOT NULL,
  message      TEXT     NOT NULL,
  args         JSON     NOT NULL,
  `from`       TEXT     NOT NULL,
  `to`         TEXT     NOT NULL,
  sent_logical_time INTEGER NOT NULL,
  at           INTEGER  NOT NULL,
  dropped      INT2     NOT NULL,
  PRIMARY KEY(test_id, run_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id),
  FOREIGN KEY(run_id)  REFERENCES run(id));

INSERT INTO network_trace_new (test_id, run_id, id, message, args, `from`, `to`, sent_logical_time, at, dropped)
SELECT test_id, run_id, id, message, args, `from`, `to`, sent_logical_time, at, dropped
  FROM network_trace;

DROP TABLE IF EXISTS network_trace;

ALTER TABLE network_trace_new
  RENAME TO network_trace;
