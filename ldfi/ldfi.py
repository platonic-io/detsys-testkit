import argparse
import sqlite3
import z3

parser = argparse.ArgumentParser(description='Lineage-driven fault injection.')
parser.add_argument('--eff', metavar='TIME', type=int, required=True,
                    help='the time when finite failures end')
parser.add_argument('--test-id', metavar='TEST_ID', type=int, required=True,
                    help='the test id')
parser.add_argument('--run-ids', metavar='RUN_ID', type=int, nargs='+', required=True,
                    help='the run ids')
args = parser.parse_args()

conn = sqlite3.connect('../db/detsys.sqlite3')
conn.row_factory = sqlite3.Row
c = conn.cursor()

sums = []
products = []

for run_id in args.run_ids:
    c.execute("select * from network_trace where test_id = (?) and run_id = (?)",
              (args.test_id, run_id))
    for r in c:
        if r['at'] < args.eff:
            sums.append("O({}, {}, {})".format(r['from'], r['to'], r['at']))
    products.append(sums)
    sums = []

c.close()

for i, sum in enumerate(products):
    products[i] = z3.Or(z3.Bools(sum))

s = z3.Solver()
s.add(z3.And(products))
s.check()
print(s.model())
