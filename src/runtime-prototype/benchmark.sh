#!/usr/bin/env bash

set -euo pipefail

# Inspired by: https://sled.rs/perf.html#experimental-design

BENCHMARK_WORKLOAD1="bench-disruptor-sp"
BENCHMARK_WORKLOAD1_GITHASH="XXX: NOT USED YET"
BENCHMARK_WORKLOAD2="bench-disruptor-tbqueue"
BENCHMARK_WORKLOAD2_GITHASH="XXX: NOT USED YET"
BENCHMARK_NUMBER_OF_RUNS=5

# Save info about current hardware and OS setup.
uname --kernel-name --kernel-release --kernel-version --machine --operating-system
echo ""
lscpu
echo ""

# Warn about non-essential programs...
FIREFOX_PID="$(pgrep GeckoMain)"
if [ -n "${FIREFOX_PID}" ]; then
    read -r -p "Firefox is running, wanna kill it? [y/N] " yn
    case $yn in
        [Yy]*) kill -1 "${FIREFOX_PID}" ;;
        *) ;;
    esac
fi

# Use the performance governor instead of powersave (for laptops).
for policy in /sys/devices/system/cpu/cpufreq/policy*; do
    echo "${policy}"
    echo "performance" | sudo tee "${policy}/scaling_governor"
done

# Compile workloads.
cabal configure "${BENCHMARK_WORKLOAD1}" \
      --disable-profiling \
      --ghc-options='-O2 -threaded -rtsopts -with-rtsopts=-N'
cabal configure "${BENCHMARK_WORKLOAD2}" \
      --disable-profiling \
      --ghc-options='-O2 -threaded -rtsopts -with-rtsopts=-N'

cabal build -O2 "${BENCHMARK_WORKLOAD1}"
cabal build -O2 "${BENCHMARK_WORKLOAD2}"

# Disable turbo boost.
echo 1 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo

# The following run is just a (CPU) warm up, the results are discarded.
cabal run -O2 ${BENCHMARK_WORKLOAD2}

# Run the benchmarks. By running workloads interleaved with each other, we
# reduce the risk of having particular transient system-wide effects impact only
# a single measurement.
for i in $(seq ${BENCHMARK_NUMBER_OF_RUNS}); do
    echo "Running benchmark run ${i}"
    perf stat -e cache-misses,branch-misses,dTLB-load-misses,iTLB-load-misses \
         cabal run -O2 "${BENCHMARK_WORKLOAD1}" \
         >> /tmp/${BENCHMARK_WORKLOAD1}.txt
    perf stat -e cache-misses,branch-misses,dTLB-load-misses,iTLB-load-misses \
         cabal run -O2 "${BENCHMARK_WORKLOAD2}" \
         >> /tmp/${BENCHMARK_WORKLOAD2}.txt

    # XXX: Can't get the below to work, ${BENCHMARK_WORKLOAD} env var doesn't
    # get interpolated correctly into the string?
    # Use `nice` to bump the priority of the benchmark process to the highest possible.
    ##sudo nice -n -20 su -c \
    ##     "perf stat -e cache-misses,branch-misses,dTLB-load-misses,iTLB-load-misses cabal run -O2 ${BENCHMARK_WORKLOAD1} >> /tmp/${BENCHMARK_WORKLOAD1}.txt" \
    ##     "${USER}"
    ##sudo nice -n -20 su -c \
    ##     "perf stat -e cache-misses,branch-misses,dTLB-load-misses,iTLB-load-misses cabal run -O2 ${BENCHMARK_WORKLOAD2} >> /tmp/${BENCHMARK_WORKLOAD2}.txt" \
    ##     "${USER}"

done

# Re-enable turbo boost.
echo 0 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo

# Output throughput data for R analysis.
R_FILE="/tmp/${BENCHMARK_WORKLOAD1}-${BENCHMARK_WORKLOAD2}.r"

echo 'Input=("' > "${R_FILE}"
echo "Workload Throughput" >> "${R_FILE}"
awk -v wl1="${BENCHMARK_WORKLOAD1}" \
    '/Throughput/ { print wl1, $2}' "/tmp/${BENCHMARK_WORKLOAD1}.txt" >> "${R_FILE}"
awk -v wl2="${BENCHMARK_WORKLOAD2}" \
    '/Throughput/ { print wl2, $2}' "/tmp/${BENCHMARK_WORKLOAD2}.txt" >> "${R_FILE}"
echo '")' >> "${R_FILE}"

cat << EOF >> "${R_FILE}"
Data = read.table(textConnection(Input),header=TRUE)
bartlett.test(Throughput ~ Workload, data=Data)

# If p-value >= 0.05, use var.equal=TRUE below

t.test(Throughput ~ Workload, data=Data,
       var.equal=TRUE,
       conf.level=0.95)
EOF

# Profiling

# On Linux install `perf` See
# https://www.kernel.org/doc/html/latest/admin-guide/perf-security.html for how
# to setup the permissions for using `perf`. Also note that on some systems,
# e.g. Ubuntu, `/usr/bin/perf` is not the actual binary but rather a bash script
# that calls the binary. Note that the steps in the admin guide needs to be
# performed on the binary and not the shell script.
#
# For more see: https://brendangregg.com/perf.html
