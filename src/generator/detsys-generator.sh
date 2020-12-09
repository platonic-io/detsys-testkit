#!/usr/bin/env bash

# Change directory to the source directory of this script. Taken from:
# https://stackoverflow.com/a/246128/3858681
pushd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )" > /dev/null

TEST="$1"
DETSYS_DB=${DETSYS_DB:-"${HOME}/.detsys.db"}

# Create test.
sqlite3 "${DETSYS_DB}" "INSERT INTO test DEFAULT VALUES"
TEST_ID=$(sqlite3 "${DETSYS_DB}" "SELECT max(id) from test")

if [ "${TEST}" == "register" ]; then
  sqlite3 "${DETSYS_DB}" <<EOF
INSERT INTO agenda (test_id, id, kind, event, args, \`from\`, \`to\`, at)
VALUES
  (${TEST_ID}, 0, "invoke", "write", '{"value": 1}', "client:0", "frontend", "1970-01-01T00:00:00Z"),
  (${TEST_ID}, 1, "invoke", "read",  "{}",           "client:0", "frontend", "1970-01-01T00:00:10Z");
INSERT INTO deployment VALUES(${TEST_ID}, "frontend", '{"inFlight":{},"inFlightSessionToClient":{},"nextSessionId":0}');
INSERT INTO deployment VALUES(${TEST_ID}, "register1", '{"value":[]}');
INSERT INTO deployment VALUES(${TEST_ID}, "register2", '{"value":[]}');
EOF
elif [ "${TEST}" == "broadcast" ]; then
    sqlite3 "${DETSYS_DB}" <<EOF
INSERT INTO deployment VALUES(${TEST_ID}, "A", '{"log":"Hello world!","broadcast":true}');
INSERT INTO deployment VALUES(${TEST_ID}, "B", '{"log":"","broadcast":false}');
INSERT INTO deployment VALUES(${TEST_ID}, "C", '{"log":"","broadcast":false}');
EOF
fi

echo "${TEST_ID}"

popd > /dev/null
