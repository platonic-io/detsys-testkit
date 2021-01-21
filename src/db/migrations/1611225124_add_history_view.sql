-- +migrate Up
CREATE VIEW IF NOT EXISTS jepsen_history AS
  SELECT
    json_extract(meta, '$.test-id') as test_id,
    json_extract(meta, '$.run-id') as run_id,
    json_extract(data, '$.jepsen-type') as kind,
    json_extract(data, '$.message') as event,
    json_extract(data, '$.args') as args,
    json_extract(data, '$.jepsen-process') as process
    FROM event_log
   WHERE event like 'NetworkTrace'
     AND json_extract(data, '$.jepsen-type') IS NOT NULL;

DROP TABLE IF EXISTS history;

-- +migrate Down
DROP VIEW IF EXISTS jepsen_history;
CREATE TABLE IF NOT EXISTS history (rowid INTEGER PRIMARY KEY) WITHOUT ROWID;
