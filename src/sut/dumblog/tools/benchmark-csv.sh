#!/usr/bin/env bash

CSV_FILE="/tmp/bench-stats.csv"

echo "workload, throughput" > "${CSV_FILE}"

declare -a CLIENTS=(5000 6000 7000 8000 9000 10000 11000 12000 13000 14000 15000)

# for i in $(seq 6 14); do
for i in "${CLIENTS[@]}"; do
    awk -v i=${i} '/Throughput/ { print("journal-"i","$2) }' \
        /tmp/bench-journal-${i}.txt >> "${CSV_FILE}"
    awk -v i=${i} '/Throughput/ { print("sqlite-"i","$2) }' \
        /tmp/bench-sqlite-${i}.txt >> "${CSV_FILE}"
done
