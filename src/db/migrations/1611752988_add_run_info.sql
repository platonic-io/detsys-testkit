-- +migrate Up
CREATE VIEW IF NOT EXISTS run_info AS
  SELECT
    json_extract(meta, '$.test-id') as test_id,
    json_extract(meta, '$.run-id') as run_id,
    json_extract(data, '$.seed') as seed,
    json_extract(data, '$.faults') as faults,
    json_extract(data, '$.tick-frequency') as tick_frequency,
    json_extract(data, '$.max-time-ns') as max_time_ns,
    json_extract(data, '$.min-time-ns') as min_time_ns
    FROM event_log
   WHERE event like 'CreateRun';

-- +migrate Down
DROP VIEW IF EXISTS run_info;
