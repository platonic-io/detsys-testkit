#!/usr/bin/env bash

DB=${DB:-"../db/detsys.sqlite3"}

# Create test.
sqlite3 "${DB}" "INSERT INTO test DEFAULT VALUES"

sqlite3 "${DB}" <<'EOF'
INSERT INTO agenda (test_id, id, kind, event, args, `from`, `to`, at)
VALUES
  (1, 0, "invoke", "write", '{"value": 1}', "client:0", "frontend", "1970-01-01T00:00:00Z"),
  (1, 1, "invoke", "read",  "{}",           "client:0", "frontend", "1970-01-01T00:00:10Z");
EOF
