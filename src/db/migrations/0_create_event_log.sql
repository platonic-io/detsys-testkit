-- +migrate Up

CREATE TABLE IF NOT EXISTS event_log (
id           INTEGER  PRIMARY KEY,
event        TEXT     NOT NULL,
meta         JSON     NOT NULL CHECK(json_valid(meta)),
data         JSON     NOT NULL CHECK(json_valid(data)),
at           DATETIME NOT NULL DEFAULT (STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW')));

-- +migrate Down
DROP TABLE IF EXISTS event_log;
