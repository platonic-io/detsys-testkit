-- +migrate Up
CREATE VIEW IF NOT EXISTS execution_step AS
  SELECT
    json_extract(meta, '$.test-id')        AS test_id,
    json_extract(meta, '$.run-id')         AS run_id,
    json_extract(data, '$.reactor')        AS reactor,
    json_extract(data, '$.logical-time')   AS logical_time,
    json_extract(data, '$.simulated-time') AS simulated_time,
    json_extract(data, '$.log-lines')      AS log_lines,
    json_extract(data, '$.diff')           AS heap_diff
  FROM event_log
  WHERE event LIKE 'ExecutionStep';

CREATE VIEW IF NOT EXISTS jepsen_history AS
  SELECT
    json_extract(meta, '$.test-id')        AS test_id,
    json_extract(meta, '$.run-id')         AS run_id,
    json_extract(data, '$.jepsen-type')    AS kind,
    json_extract(data, '$.message')        AS event,
    json_extract(data, '$.args')           AS args,
    json_extract(data, '$.jepsen-process') AS process
  FROM event_log
  WHERE event LIKE 'NetworkTrace'
  AND json_extract(data, '$.jepsen-type') IS NOT NULL;

CREATE VIEW IF NOT EXISTS network_trace AS
  SELECT
    json_extract(meta, '$.test-id')             AS test_id,
    json_extract(meta, '$.run-id')              AS run_id,
    json_extract(data, '$.message')             AS message,
    json_extract(data, '$.args')                AS args,
    json_extract(data, '$.from')                AS sender,
    json_extract(data, '$.to')                  AS receiver,
    json_extract(data, '$.kind')                AS kind,
    json_extract(data, '$.sent-logical-time')   AS sent_logical_time,
    json_extract(data, '$.recv-logical-time')   AS recv_logical_time,
    json_extract(data, '$.recv-simulated-time') AS recv_simulated_time,
    json_extract(data, '$.dropped')             AS dropped
  FROM event_log
  WHERE event LIKE 'NetworkTrace';

CREATE VIEW IF NOT EXISTS run_info AS
  SELECT
    json_extract(meta, '$.test-id')        AS test_id,
    json_extract(meta, '$.run-id')         AS run_id,
    json_extract(data, '$.seed')           AS seed,
    json_extract(data, '$.faults')         AS faults,
    json_extract(data, '$.tick-frequency') AS tick_frequency,
    json_extract(data, '$.max-time-ns')    AS max_time_ns,
    json_extract(data, '$.min-time-ns')    AS min_time_ns
  FROM event_log
  WHERE event LIKE 'CreateRun';

CREATE VIEW IF NOT EXISTS test_info AS
  SELECT
    json_extract(meta, '$.test-id')    AS test_id,
    json_extract(data, '$.agenda')     AS agenda,
    json_extract(data, '$.deployment') AS deployment,
    at                                 AS created_time
  FROM event_log
  WHERE event LIKE 'CreateTest';

-- +migrate Down
DROP VIEW IF EXISTS execution_step;
DROP VIEW IF EXISTS jepsen_history;
DROP VIEW IF EXISTS network_trace;
DROP VIEW IF EXISTS run_info;
DROP VIEW IF EXISTS test_info;
