import argparse
import json
import logging
import os
import sqlite3
import z3
from pkg_resources import get_distribution

# Logging
logging.basicConfig(filename='/tmp/ldfi.log',
                    filemode='w',
                    level=logging.DEBUG)

def order(d):
    return("%s %s %s %d" % (d['kind'], d['from'], d.get('to', ""), d['at']))

def main():
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
    parser.add_argument('--version', '-v', action='version',
                        version=get_distribution(__name__).version)

    args = parser.parse_args()

    # Load network traces from the database.
    db = os.getenv("DETSYS_DB", os.getenv("HOME") + "/.detsys.db")
    conn = sqlite3.connect(db)
    conn.row_factory = sqlite3.Row
    c = conn.cursor()

    products = []
    crashes = set()
    previous_faults = []

    for run_id in args.run_ids:
        prods = []
        c.execute("""SELECT faults FROM faults
                     WHERE test_id = (?)
                       AND run_id = (?)""", (args.test_id, run_id))
        for r in c:
            for fault in eval(r['faults']):
                # NOTE: eval introduces a space after the colon in a
                # dict, we need to remove this otherwise the variables
                # of the SAT expression will differ.
                prods.append(str(fault).replace(": ", ":"))
        previous_faults.append(prods)

        sums = []
        c.execute("""SELECT * FROM network_trace
                     WHERE test_id = (?)
                       AND run_id = (?)
                       AND kind <> 'timer'
                       AND NOT (`from` LIKE 'client:%')
                       AND NOT (`to`   LIKE 'client:%')""",
                  (args.test_id, run_id))
        for r in c:
            if r['at'] < args.eff:
                sums.append("{'kind':'omission', 'from':'%s', 'to':'%s', 'at':%d}" %
                             (r['from'], r['to'], r['at']))
            if args.crashes > 0:
                crash = "{'kind':'crash', 'from':'%s', 'at':%d}" % (r['from'], r['at'])
                sums.append(crash)
                crashes.add(crash)
        products.append(sums)

    # Sanity check.
    for i, run_id in enumerate(args.run_ids):
        if not products[i] and not crashes:
            print("Error: couldn't find a network trace for test id: %d, and run id: %d." %
                  (args.test_id, run_id))
            exit(1)

    # Create and solve SAT formula.
    for i, sum in enumerate(products):
        products[i] = z3.Or(z3.Bools(sum))

    for i, prod in enumerate(previous_faults):
        if prod:
            previous_faults[i] = z3.And(z3.Bools(prod))
        else:
            previous_faults[i] = False

    crashes = z3.Bools(list(crashes))

    s = z3.Solver()
    s.add(z3.And(products))

    # We don't want to injects faults that we already tried injecting in previous runs.
    logging.debug(str(z3.Not(z3.Or(previous_faults))))
    s.add(z3.Not(z3.Or(previous_faults)))

    # There can be at most --crashes many crashes.
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

        statistics = {}
        for k, v in s.statistics():
            statistics[k] = v

        if not(args.json):
            print(m)
            print(statistics)
        else:
            faults = []
            for d in m.decls():
                if m[d]:
                    Dict = eval(d.name())
                    faults.append(Dict)
            faults = sorted(faults, key=order)

            c.execute("""INSERT INTO faults(test_id, run_id, faults, version, statistics)
                         VALUES(?, ?, ?, ?, ?)""",
                      (args.test_id, args.run_ids[-1], json.dumps(faults),
                       get_distribution(__name__).version, str(statistics)))

            conn.commit()

            c.close()

            print(json.dumps({"faults": faults,
                              "statistics": statistics,
                              "version": get_distribution(__name__).version}))

if __name__ == '__main__':
    main()
