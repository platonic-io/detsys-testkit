#!/usr/bin/env python3

import argparse
from datetime import datetime
import os
import sqlite3
import z3
import json

# Command-line argument parsing.
parser = argparse.ArgumentParser(description='Lineage-driven fault injection.')
parser.add_argument('--eff', metavar='TIME', type=str, required=True,
                    help='''the time when finite failures end, either expressed as relative time (an
                    integer) or absolute time (e.g.
                    1970-01-01T00:00:00.000000000Z)''')
parser.add_argument('--crashes', metavar='INT', type=int, required=True,
                    help='the max amount of node crashes')
parser.add_argument('--test-id', metavar='TEST_ID', type=int, required=True,
                    help='the test id')
parser.add_argument('--run-ids', metavar='RUN_ID', type=int, nargs='+', required=True,
                    help='the run ids')
parser.add_argument('--json', action='store_true', help='output in JSON format?')
args = parser.parse_args()

# Load network traces from the database.
db = os.path.abspath(os.path.join(os.path.dirname(__file__),
                                  '..', 'db', 'detsys.sqlite3'))
conn = sqlite3.connect(db)
conn.row_factory = sqlite3.Row
c = conn.cursor()

# Comparison of relative or absolute time.
def less_than(t1, t2):
    if t1.isdigit() and t2.isdigit():
        return(int(t1) < int(t2))
    else:
        # Datetime in the database is of the following format:
        #
        #   1970-01-01T00:00:00.017828020Z
        #
        # But Python cannot handle nanoseconds, so we truncate the last four
        # characters.
        t1 = t1[:-4]
        t2 = t2[:-4]
        return (datetime.fromisoformat(t1) < datetime.fromisoformat(t2))

products = []
crashes = set()

for run_id in args.run_ids:
    sums = []
    c.execute("select * from network_trace where test_id = (?) and run_id = (?)",
              (args.test_id, run_id))
    for r in c:
        if less_than(r['at'], args.eff):
            sums.append("{'kind':'omission', 'from':'%s', 'to':'%s', 'at':%s}" %
                        (r['from'], r['to'], str(r['at'])))
        crash = "{'kind':'crash', 'from':'%s', 'at':%s}" % (r['from'], str(r['at']))
        sums.append(crash)
        crashes.add(crash)
    products.append(sums)

c.close()

# Sanity check.
for i, run_id in enumerate(args.run_ids):
    if not products[i]:
        print("Error: couldn't find a network trace for test id: %d, and run id: %d." %
              (args.test_id, run_id))
        exit(1)

# Create and solve SAT formula.
for i, sum in enumerate(products):
    products[i] = z3.Or(z3.Bools(sum))

crashes = z3.Bools(list(crashes))

s = z3.Solver()
s.add(z3.And(products))
if crashes:
    crashes.append(args.crashes)
    s.add(z3.AtMost(crashes))
r = s.check()

# Output the result.
if r == z3.unsat:
    if not(args.json):
        print("No further faults can be injected at this point, the test case is")
        print("certified for this particular failure specification!")
    else:
        print(json.dumps({"faults": []}))
elif r == z3.unknown:
         print("Impossible: the SAT solver returned 'unknown'")
         try:
             print(s.model())
         except Z3Exception:
             pass
         finally:
             exit(2)
else:
    m = s.model()

    if not(args.json):
        print(m)
    else:
        faults = []
        for d in m.decls():
            if m[d]:
                Dict = eval(d.name())
                faults.append(Dict)
        print(json.dumps({"faults": faults}))
