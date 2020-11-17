-- +migrate Up

ALTER TABLE network_trace
  ADD kind TEXT;

UPDATE network_trace
   SET kind = "unknown"
 WHERE kind IS NULL;

-- +migrate Down

ALTER TABLE network_trace
  DROP COLUMN kind;
