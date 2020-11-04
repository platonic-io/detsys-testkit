#!/usr/bin/env bash

# Change directory to the source directory of this script. Taken from:
# https://stackoverflow.com/a/246128/3858681
pushd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )" > /dev/null

DB=${DB:-"../db/detsys.sqlite3"}

# Create test.
sqlite3 "${DB}" "INSERT INTO test DEFAULT VALUES"
TEST_ID=$(sqlite3 "${DB}" "SELECT max(id) from test")

sqlite3 "${DB}" <<EOF
INSERT INTO agenda (test_id, id, kind, event, args, \`from\`, \`to\`, at)
VALUES
  (${TEST_ID}, 0, "invoke", "write", '{"value": 1}', "client:0", "frontend", "1970-01-01T00:00:00Z"),
  (${TEST_ID}, 1, "invoke", "read",  "{}",           "client:0", "frontend", "1970-01-01T00:00:10Z");
INSERT INTO deployment VALUES(1, "frontend", '{"inFlight":{},"inFlightSessionToClient":{},"nextSessionId":0}');
INSERT INTO deployment VALUES(1, "register1", '{"value":[]}');
INSERT INTO deployment VALUES(1, "register2", '{"value":[]}');
EOF

echo "${TEST_ID}"

popd > /dev/null
