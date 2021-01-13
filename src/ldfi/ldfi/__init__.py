import argparse
import json
import logging
import os
import sqlite3
import z3
from typing import (List, Set, Tuple)
from pkg_resources import get_distribution

# Logging
logging.basicConfig(filename='/tmp/ldfi.log',
                    filemode='w',
                    level=logging.DEBUG)

class Storage:
    def load_previous_faults(self, test_id: int, run_ids: List[int]) -> List[List[str]]:
        pass
    def load_potential_faults(self, test_id: int, run_ids: List[int],
                              eff: int, max_crashes: int,
                              previous_faults: List[List[str]]) -> Tuple[List[List[str]], Set[str]]:
        pass
    def store_faults(self, test_id: int, run_id: int, faults: str,
                     version: str, statistics: str):
        pass

class SqliteStorage(Storage):

    def __init__(self):
        self.db = os.getenv("DETSYS_DB", os.getenv("HOME") + "/.detsys.db")
        self.conn = sqlite3.connect(self.db)
        self.conn.row_factory = sqlite3.Row
        self.c = self.conn.cursor()

    def load_previous_faults(self, test_id: int, run_ids: List[int]) -> List[List[str]]:
        previous_faults = []

        for run_id in run_ids:
            prods = []
            self.c.execute("""SELECT faults FROM faults
                              WHERE test_id = (?)
                              AND run_id = (?)""", (test_id, run_id))
            for r in self.c:
                for fault in eval(r['faults']):
                    # NOTE: eval introduces a space after the colon in a
                    # dict, we need to remove this otherwise the variables
                    # of the SAT expression will differ.
                    prods.append(str(fault).replace(": ", ":"))
                    logging.debug("fault: '%s'", str(fault).replace(": ", ":"))
            previous_faults.append(prods)

        return previous_faults

    def load_potential_faults(self, test_id: int, run_ids: List[int],
                              eff: int, max_crashes: int,
                              previous_faults: List[List[str]]) -> Tuple[List[List[str]], Set[str]]:
        potential_faults = []
        crashes = set()

        for run_id in run_ids:
            sums = []
            self.c.execute("""SELECT * FROM network_trace
                              WHERE test_id = (?)
                                AND run_id = (?)
                                AND kind <> 'timer'
                                AND NOT (`from` LIKE 'client:%')
                                AND NOT (`to`   LIKE 'client:%')""",
                           (test_id, run_id))
            for r in self.c:
                if r['at'] < eff:
                    omission = ("{'kind':'omission', 'from':'%s', 'to':'%s', 'at':%d}"
                                % (r['from'], r['to'], r['at']))
                    if omission not in previous_faults:
                        sums.append(omission)
                if max_crashes > 0:
                    crash = ("{'kind':'crash', 'from':'%s', 'at':%d}"
                             % (r['from'], r['sent_logical_time']))
                    if crash not in previous_faults:
                        sums.append(crash)
                        crashes.add(crash)
            potential_faults.append(sums)

        return (potential_faults, crashes)

    def store_faults(self, test_id: int, run_id: int, faults: str,
                     version: str, statistics: str):
            self.c.execute("""INSERT INTO faults(test_id, run_id, faults, version, statistics)
                         VALUES(?, ?, ?, ?, ?)""",
                      (test_id, run_id, faults, version, statistics))

            self.conn.commit()
            self.c.close()

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
    storage = SqliteStorage()
    previous_faults = storage.load_previous_faults(args.test_id, args.run_ids)
    logging.debug(str(previous_faults))
    (products, crashes) = storage.load_potential_faults(args.test_id, args.run_ids, args.eff,
                                                        args.crashes, previous_faults)

    # Sanity check.
    for i, run_id in enumerate(args.run_ids):
        if not products[i] and not crashes:
            print("Error: couldn't find a network trace for test id: %d, and run id: %d." %
                  (args.test_id, run_id))
            exit(1)

    # Create and solve SAT formula.
    for i, sum in enumerate(products):
        kept = z3.Bools(sum)
        logging.debug("i: %d", i)
        logging.debug("kept: " + str(kept))
        if previous_faults[i]:
            drop = z3.Bools(previous_faults[i])
            logging.debug("drop: " + str(drop))
        else:
            drop = False
        products[i] = z3.Or(z3.Or(kept), z3.Not(z3.And(drop)))

    crashes = z3.Bools(list(crashes))

    s = z3.Solver()
    s.add(z3.And(products))

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

            storage.store_faults(args.test_id, args.run_ids[-1], json.dumps(faults),
                                 get_distribution(__name__).version, str(statistics))

            print(json.dumps({"faults": faults,
                              "statistics": statistics,
                              "version": get_distribution(__name__).version}))

if __name__ == '__main__':
    main()
