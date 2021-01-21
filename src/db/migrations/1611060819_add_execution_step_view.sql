-- +migrate Up
CREATE VIEW IF NOT EXISTS execution_step AS
  SELECT
    json_extract(meta, '$.test-id') as test_id,
    json_extract(meta, '$.run-id') as run_id,
    json_extract(data, '$.reactor') as reactor,
    json_extract(data, '$.logical-time') as logical_time,
    json_extract(data, '$.simulated-time') as simulated_time,
    json_extract(data, '$.log-lines') as log_lines,
    json_extract(data, '$.diff') as heap_diff
    FROM event_log
           WHERE event like 'ExecutionStep';

-- +migrate Down
DROP VIEW IF EXISTS execution_step;
