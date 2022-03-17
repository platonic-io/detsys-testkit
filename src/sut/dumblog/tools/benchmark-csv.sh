#!/usr/bin/env bash

CSV_FILE="/tmp/bench-stats.csv"

echo "workload, throughput" > "${CSV_FILE}"

for i in $(seq 6 14); do
    awk -v i=${i} '/Throughput/ { print("journal-"i","$2) }' \
        /tmp/bench-journal-${i}.txt >> "${CSV_FILE}"
    awk -v i=${i} '/Throughput/ { print("sqlite-"i","$2) }' \
        /tmp/bench-sqlite-${i}.txt >> "${CSV_FILE}"
done
