-- +migrate Up
DROP TABLE IF EXISTS network_trace;
DROP TABLE IF EXISTS time_mapping;

CREATE VIEW IF NOT EXISTS network_trace AS
  SELECT
    json_extract(meta, '$.test-id') as test_id,
    json_extract(meta, '$.run-id') as run_id,
    json_extract(data, '$.message') as message,
    json_extract(data, '$.args') as args,
    json_extract(data, '$.from') as sender,
    json_extract(data, '$.to') as receiver,
    json_extract(data, '$.kind') as kind,
    json_extract(data, '$.sent-logical-time') as sent_logical_time,
    json_extract(data, '$.recv-logical-time') as recv_logical_time,
    json_extract(data, '$.recv-simulated-time') as recv_simulated_time,
    json_extract(data, '$.dropped') as dropped
    FROM event_log
   WHERE event like 'NetworkTrace';

-- +migrate Down

DROP VIEW IF EXISTS network_trace;
CREATE TABLE IF NOT EXISTS time_mapping (rowid INTEGER PRIMARY KEY) WITHOUT ROWID;

-- we need to create this table fully since we have migrations that actually do stuff with
-- this table...
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
