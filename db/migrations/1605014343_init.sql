-- +migrate Up
CREATE TABLE IF NOT EXISTS test (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  create_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);

CREATE TABLE IF NOT EXISTS agenda (
  test_id      INTEGER  NOT NULL,
  id           INTEGER  NOT NULL,
  kind         TEXT     NOT NULL CHECK(kind IN ("invoke", "fault", "message")),
  event        TEXT     NOT NULL,
  args         JSON     NOT NULL,
  `from`       TEXT     NOT NULL,
  `to`         TEXT     NOT NULL,
  at           DATETIME NOT NULL,
  PRIMARY KEY(test_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id));

CREATE TABLE IF NOT EXISTS run (
  test_id       INTEGER  NOT NULL,
  id            INTEGER  NOT NULL,
  seed          INTEGER  NOT NULL,
  create_time   DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY(test_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id));

CREATE TABLE IF NOT EXISTS history (
  test_id      INTEGER  NOT NULL,
  run_id       INTEGER  NOT NULL,
  id           INTEGER  NOT NULL,
  kind         TEXT     NOT NULL CHECK(kind IN ("invoke", "ok", "fail", "info")),
  event        TEXT     NOT NULL,
  args         JSON     NOT NULL,
  process      INTEGER  NOT NULL,
  PRIMARY KEY(test_id, run_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id),
  FOREIGN KEY(run_id)  REFERENCES run(id));

CREATE TABLE IF NOT EXISTS network_trace (
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

CREATE TABLE IF NOT EXISTS heap_trace (
  test_id      INTEGER   NOT NULL,
  run_id       INTEGER   NOT NULL,
  id           INTEGER   NOT NULL,
  component    TEXT      NOT NULL,
  heap         JSON      NOT NULL,
  at           DATETIME  NOT NULL,
  PRIMARY KEY(test_id, run_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id),
  FOREIGN KEY(run_id)  REFERENCES run(id));

CREATE TABLE IF NOT EXISTS deployment (
  test_id      INTEGER   NOT NULL,
  component    TEXT      NOT NULL,
  args         JSON      NOT NULL,
  PRIMARY KEY(test_id, component),
  FOREIGN KEY(test_id) REFERENCES test(id));

CREATE TABLE IF NOT EXISTS analysis (
  test_id      INTEGER   NOT NULL,
  run_id       INTEGER   NOT NULL,
  id           INTEGER   NOT NULL,
  valid        INT2      NOT NULL,
  result       JSON      NOT NULL,
  PRIMARY KEY(test_id, run_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id),
  FOREIGN KEY(run_id)  REFERENCES run(id));

-- +migrate Down
DROP TABLE IF EXISTS test;
DROP TABLE IF EXISTS agenda;
DROP TABLE IF EXISTS run;
DROP TABLE IF EXISTS history;
DROP TABLE IF EXISTS network_trace;
DROP TABLE IF EXISTS heap_trace;
DROP TABLE IF EXISTS deployment;
DROP TABLE IF EXISTS analysis;
