import argparse
import sqlite3
import z3
import json

# Command-line argument parsing.
parser = argparse.ArgumentParser(description='Lineage-driven fault injection.')
parser.add_argument('--eff', metavar='TIME', type=int, required=True,
                    help='the time when finite failures end')
parser.add_argument('--crashes', metavar='INT', type=int, required=True,
                    help='the max amount of node crashes')
parser.add_argument('--test-id', metavar='TEST_ID', type=int, required=True,
                    help='the test id')
parser.add_argument('--run-ids', metavar='RUN_ID', type=int, nargs='+', required=True,
                    help='the run ids')
parser.add_argument('--json', action='store_true', help='output in JSON format?')
args = parser.parse_args()

# Load network traces from the database.
conn = sqlite3.connect('../db/detsys.sqlite3')
conn.row_factory = sqlite3.Row
c = conn.cursor()

products = []
crashes = set()

for run_id in args.run_ids:
    sums = []
    c.execute("select * from network_trace where test_id = (?) and run_id = (?)",
              (args.test_id, run_id))
    for r in c:
        if r['at'] < args.eff:
            sums.append("{'kind':'omission', 'from':'%s', 'to':'%s', 'at':%d}" %
                        (r['from'], r['to'], r['at']))
        crash = "{'kind':'crash', 'from':'%s', 'at':%d}" % (r['from'], r['at'])
        sums.append(crash)
        crashes.add(crash)
    products.append(sums)

c.close()

# Create and solve SAT formula.
for i, sum in enumerate(products):
    products[i] = z3.Or(z3.Bools(sum))

crashes = z3.Bools(list(crashes))

s = z3.Solver()
s.add(z3.And(products))
crashes.append(args.crashes)
s.add(z3.AtMost(crashes))
s.check()
m = s.model()

# Output the result.
if not(args.json):
    print(m)
else:
    faults = []
    for d in m.decls():
        if m[d]:
            Dict = eval(d.name())
            faults.append(Dict)

    print(json.dumps({"faults": faults}))
