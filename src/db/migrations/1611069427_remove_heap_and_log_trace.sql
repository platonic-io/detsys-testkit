-- +migrate Up
DROP TABLE IF EXISTS heap_trace;
DROP TABLE IF EXISTS log_trace;

-- +migrate Down
CREATE TABLE IF NOT EXISTS log_trace (rowid INTEGER PRIMARY KEY) WITHOUT ROWID;
CREATE TABLE IF NOT EXISTS heap_trace (rowid INTEGER PRIMARY KEY) WITHOUT ROWID;