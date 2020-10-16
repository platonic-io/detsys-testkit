CREATE TABLE test (
  id          INTEGER PRIMARY KEY AUTOINCREMENT,
  create_time DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP);

CREATE TABLE agenda (
  test_id      INTEGER  NOT NULL,
  id           INTEGER  NOT NULL,
  kind         TEXT     NOT NULL CHECK(kind IN ("invoke", "fault", "message")),
  event        TEXT     NOT NULL,
  args         JSON     NOT NULL,
  `from`       TEXT     NOT NULL,
  `to`         TEXT     NOT NULL,
  at           INTEGER  NOT NULL,
  PRIMARY KEY(test_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id));

CREATE TABLE run (
  test_id       INTEGER  NOT NULL,
  id            INTEGER  NOT NULL,
  seed          INTEGER  NOT NULL,
  create_time   DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY(test_id, id),
  FOREIGN KEY(test_id) REFERENCES test(id));

CREATE TABLE history (
  run_id       INTEGER  NOT NULL,
  id           INTEGER  NOT NULL,
  kind         TEXT     NOT NULL CHECK(kind IN ("invoke", "ok", "fail", "info")),
  event        TEXT     NOT NULL,
  args         JSON     NOT NULL,
  process      INTEGER  NOT NULL,
  PRIMARY KEY(run_id, id),
  FOREIGN KEY(run_id) REFERENCES run(id));
