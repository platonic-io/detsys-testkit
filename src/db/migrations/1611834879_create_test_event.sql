-- +migrate Up
CREATE VIEW IF NOT EXISTS test_info AS
  SELECT
    json_extract(meta, '$.test-id') as test_id,
    json_extract(data, '$.agenda') as agenda,
    json_extract(data, '$.deployment') as deployment,
    at as created_time
    FROM event_log
   WHERE event like 'CreateTest';

DROP TABLE IF EXISTS deployment;
DROP TABLE IF EXISTS agenda;
DROP TABLE IF EXISTS test;

-- +migrate Down
DROP VIEW IF EXISTS test_info;

CREATE TABLE IF NOT EXISTS test (rowid INTEGER PRIMARY KEY) WITHOUT ROWID;
CREATE TABLE IF NOT EXISTS agenda (rowid INTEGER PRIMARY KEY) WITHOUT ROWID;
CREATE TABLE IF NOT EXISTS deployment (rowid INTEGER PRIMARY KEY) WITHOUT ROWID;
