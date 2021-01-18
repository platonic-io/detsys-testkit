import argparse
import json
import logging
import os
import sqlite3
import z3
from typing import (List, Set)
from pkg_resources import get_distribution

class Config:
    def __init__(self,
                 test_id: int,
                 run_ids: List[int],
                 eff: int,
                 max_crashes: int):
      self.test_id = test_id
      self.run_ids = run_ids
      self.eff = eff
      self.max_crashes = max_crashes

      # TODO(stevan): make logging configurable.
      logging.basicConfig(level=logging.DEBUG)

def create_config() -> Config:
    parser = argparse.ArgumentParser(description='Lineage-driven fault injection.')
    parser.add_argument('--eff', metavar='TIME', type=int, required=True,
                        help='the time when finite failures end')
    parser.add_argument('--crashes', metavar='INT', type=int, required=True,
                        help='the max amount of node crashes')
    parser.add_argument('--test-id', metavar='TEST_ID', type=int, required=True,
                        help='the test id')
    parser.add_argument('--run-ids', metavar='RUN_ID', type=int, nargs='+', required=True,
                        help='the run ids')
    parser.add_argument('--version', '-v', action='version',
                        version=get_distribution(__name__).version)

    args = parser.parse_args()

    return Config(args.test_id, args.run_ids, args.eff, args.crashes)

class Data:
    def __init__(self,
                 previous_faults: List[List[str]],
                 potential_faults: List[List[str]],
                 crashes: Set[str]):
        self.previous_faults = previous_faults
        self.potential_faults = potential_faults
        self.crashes = crashes

class Event:
    def __init__(self,
                 test_id: int,
                 run_id: int,
                 faults: str,
                 version: str,
                 statistics: str):
        self.test_id = test_id
        self.run_id = run_id
        self.faults = faults
        self.version = version
        self.statistics = statistics

class Storage:
    def load(self, config: Config) -> Data:
        pass

    def store(self, event: Event):
        pass

class SqliteStorage(Storage):

    def __init__(self):
        self.db = os.getenv("DETSYS_DB", os.getenv("HOME") + "/.detsys.db")
        self.conn = sqlite3.connect(self.db)
        self.conn.row_factory = sqlite3.Row
        self.c = self.conn.cursor()

    def load(self, config: Config) -> Data:
        previous_faults = []
        potential_faults = []
        crashes = set()

        for run_id in config.run_ids:
            # The last run id will not have any previous faults.
            if run_id != config.run_ids[-1]:
                prods = []
                self.c.execute("""SELECT faults FROM faults
                                  WHERE test_id = (?)
                                  AND run_id = (?)""", (config.test_id, run_id))
                for r in self.c:
                    for fault in eval(r['faults'])['faults']:
                        # NOTE: eval introduces a space after the colon in a
                        # dict, we need to remove this otherwise the variables
                        # of the SAT expression will differ.
                        prods.append(str(fault).replace(": ", ":"))
                        logging.debug("fault: '%s'", str(fault).replace(": ", ":"))
                        previous_faults.append(prods)

            sums = []
            logging.debug("previous_faults: %s", previous_faults)
            self.c.execute("""SELECT * FROM network_trace
                              WHERE test_id = (?)
                                AND run_id = (?)
                                AND kind <> 'timer'
                                AND NOT (`from` LIKE 'client:%')
                                AND NOT (`to`   LIKE 'client:%')""",
                           (config.test_id, run_id))
            for r in self.c:
                logging.debug("network trace: {'kind': %s, 'from': %s, 'to': %s, 'at': %s}",
                              r['kind'], r['from'], r['to'], r['at'])
                if r['at'] < config.eff:
                    omission = ("{'kind':'omission', 'from':'%s', 'to':'%s', 'at':%d}"
                                % (r['from'], r['to'], r['at']))
                    if omission not in previous_faults:
                        logging.debug("added: %s", omission)
                        sums.append(omission)
                if config.max_crashes > 0:
                    crash = ("{'kind':'crash', 'from':'%s', 'at':%d}"
                             % (r['from'], r['sent_logical_time']))
                    if crash not in previous_faults:
                        sums.append(crash)
                        crashes.add(crash)
            potential_faults.append(sums)

        logging.debug("potential_faults: %s", potential_faults)
        return Data(previous_faults, potential_faults, crashes)

    def store(self, event: Event):
        self.c.execute("""INSERT INTO faults(test_id, run_id, faults, version, statistics)
                          VALUES(?, ?, ?, ?, ?)""",
                       (event.test_id, event.run_id, event.faults, event.version,
                        event.statistics))
        self.conn.commit()

def sanity_check(data):
    if data.previous_faults == [[]]:
        len_previous_faults = 0
    else:
        len_previous_faults = len(data.previous_faults)

    logging.debug("data.previous_faults: %s (len: %d)",
                  data.previous_faults, len_previous_faults)
    logging.debug("data.potential_faults: %s (len: %d)",
                  data.potential_faults, len(data.potential_faults) + 1)

    assert(len(data.potential_faults) == len_previous_faults + 1)

    # previous_faults = [[oab1]]
    # potential_faults = [[oab1, oac1], [oac1]]
    # data.potential_faults[-1] + data.previous_faults[-1] = data.potential_faults[-2]

def create_sat_formula(config, data):
    potential_faults = []
    for i, sum in enumerate(data.potential_faults):
        logging.debug("i: %d", i)
        kept = z3.Bools(sum)
        logging.debug("kept: " + str(kept))
        drop = []
        if i != 0 and data.previous_faults[i-1]:
            drop = z3.Bools(data.previous_faults[i-1])
            logging.debug("drop: " + str(drop))
        if drop:
            potential_faults.append(z3.Or(z3.Or(kept),
                                          z3.Not(z3.And(drop))))
        else:
            potential_faults.append(z3.Or(kept))

    formula = z3.And(potential_faults)

    crashes = z3.Bools(list(data.crashes))

    if crashes:
        crashes.append(config.max_crashes)
        formula = z3.And(formula, z3.AtMost(crashes))
    logging.debug("formula: %s", str(formula))
    return formula

def sat_solve(formula):
    solver = z3.Solver()
    solver.add(formula)
    result = solver.check()
    if result == z3.sat:
        model = solver.model()
        statistics = solver.statistics()
    else:
        model = None
        statistics = None
    return (result, model, statistics)


def order(d: dict) -> str:
    return("%s %s %s %d" % (d['kind'], d['from'], d.get('to', ""), d['at']))

def create_log_event(config, result, model, statistics) -> Event:
    statistics_dict = {}
    for k, v in statistics:
        statistics_dict[k] = v

    event = Event(config.test_id, config.run_ids[-1], json.dumps({"faults": []}),
                  get_distribution(__name__).version, str(statistics_dict))

    if result == z3.unsat:
        # No further faults can be injected at this point, the test case is
        # certified for this particular failure specification!
        return event
    elif result == z3.unknown:
        logging.critical("Impossible: the SAT solver returned 'unknown'")
        try:
            logging.critical(model)
        except z3.Z3Exception:
            pass
        finally:
            exit(2)
    else:
        faults = []
        for d in model.decls():
            if model[d]:
                Dict = eval(d.name())
                faults.append(Dict)
        faults = sorted(faults, key=order)
        event.faults = json.dumps({"faults": faults})

        return event

def main():
    config = create_config()
    storage = SqliteStorage()

    data = storage.load(config)
    sanity_check(data)

    formula = create_sat_formula(config, data)

    (result, model, statistics) = sat_solve(formula)

    event = create_log_event(config, result, model, statistics)
    storage.store(event)
    logging.debug(event.faults)
    print(event.faults)

if __name__ == '__main__':
    main()
