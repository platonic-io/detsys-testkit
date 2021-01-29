-- +migrate Up

CREATE TABLE IF NOT EXISTS event_log (
id           INTEGER  PRIMARY KEY,
event        TEXT     NOT NULL,
meta         JSON     NOT NULL,
data         JSON     NOT NULL,
at           DATETIME NOT NULL DEFAULT (STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')));

-- +migrate Down

-- No down migration to avoid dropping the event log by mistake.
