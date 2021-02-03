import argparse
import json
import logging
import os
import sqlite3
import z3
from typing import (List, Set, Dict)
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
      #logging.basicConfig(level=logging.DEBUG)

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
    def load_previous_faults(self, config: Config) -> List[List[Dict]]:
        pass
    def load_potential_faults(self, config: Config) -> List[List[Dict]]:
        pass
    def store(self, event: Event):
        pass

class SqliteStorage(Storage):

    def __init__(self):
        self.db = os.getenv("DETSYS_DB", os.getenv("HOME") + "/.detsys.db")
        self.conn = sqlite3.connect(self.db)
        self.conn.row_factory = sqlite3.Row
        self.c = self.conn.cursor()

    def load_previous_faults(self, config: Config) -> List[List[Dict]]:
        # The json is not guaranteed to be in the same order as the dictionary
        # we normally create, so we re-create it here to guarantee they will be
        # the same.
        def element(e):
            return {"kind": e['kind'],
                    "from": e['from'],
                    "to": e['to'],
                    "at": e['at']}
        self.c.execute("""SELECT faults FROM run_info
                          WHERE test_id = %s
                          ORDER BY run_id ASC""" % config.test_id)

        return [ [ element(e) for e in json.loads(row["faults"])] for row in self.c.fetchall() ]

    def load_potential_faults(self, config: Config) -> List[List[Dict]]:
        potential_faults: List[List[Dict]] = [ [] for _ in range(len(config.run_ids)) ]
        self.c.execute("""SELECT run_id,sender,receiver,recv_logical_time,sent_logical_time FROM network_trace
                          WHERE test_id = %d
                          AND kind <> 'timer'
                          AND NOT (sender LIKE 'client:%%')
                          AND NOT (receiver   LIKE 'client:%%')
                          ORDER BY run_id ASC""" % config.test_id)
        i = 0
        run_id = config.run_ids[0]
        for row in self.c.fetchall():
            if row["run_id"] != run_id:
                run_id = row["run_id"]
                i += 1
            potential_faults[i].append(
                {"from": row["sender"],
                 "to": row["receiver"],
                 "at": int(row["recv_logical_time"]),
                 "sent_logical_time": int(row["sent_logical_time"])})

        return potential_faults

    def store(self, event: Event):
        meta = json.dumps({"component": "ldfi",
                           "test-id": event.test_id,
                           "run-id": event.run_id})
        data = json.dumps({"faults": json.loads(event.faults)["faults"],
                           "version": event.version,
                           "statistics": event.statistics})
        self.c.execute("""INSERT INTO event_log(event, meta, data)
                          VALUES(?, ?, ?)""",
                       ("LdfiAnalysis", meta, data))
        self.conn.commit()

def create_sat_formula(config, previous_faults, potential_faults):
    crashes = set()
    relevant_faults = []

    logging.debug("previous_faults: %s", str(previous_faults))

    for i, faults in enumerate(potential_faults):
        relevant_faults_in_run = []
        for fault in faults:
            logging.debug("fault: %s", str(fault))
            if fault['at'] < config.eff:
                omission = {'kind': 'omission',
                            'from': fault['from'],
                            'to': fault['to'],
                            'at': fault['at']}
                if i == 0 or omission not in previous_faults[i-1]:
                    logging.debug("found relevant fault: %s", omission)
                    relevant_faults_in_run.append(omission)

                if config.max_crashes > 0:
                    crash = {'kind': 'crash',
                             'from': fault['from'],
                             'at': fault['at']} # TODO(stevan): or sent_logical_time?
                    if i == 0 or crash not in previous_faults[i-1]:
                        relevant_faults_in_run.append(crash)
                        crashes.add(str(crash))
        relevant_faults.append(relevant_faults_in_run)

    formula_for_run = []

    for i, relevant_faults_in_run in enumerate(relevant_faults):
        logging.debug("i: %d", i)
        kept = z3.Bools(map(str, relevant_faults_in_run))
        logging.debug("kept: %s", str(kept))
        drop = []
        if i != 0 and previous_faults[i-1]:
            drop = z3.Bools(map(str, previous_faults[i-1]))
            logging.debug("drop: %s", str(drop))
        if drop:
            formula_for_run.append(z3.Or(z3.Or(kept),
                                         z3.Not(z3.And(drop))))
        else:
            formula_for_run.append(z3.Or(kept))

    formula = z3.And(formula_for_run)

    crashes = z3.Bools(list(crashes))

    if crashes:
        crashes.append(config.max_crashes)
        formula = z3.And(formula, z3.AtMost(crashes))
    logging.debug("formula: %s", str(formula))
    return formula

def sat_solve(formula):
    solver = z3.Solver()
    solver.add(formula)
    # TODO(stevan): Use seed from the run?
    z3.set_option('smt.random_seed', 0)
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
    if statistics:
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
                dictionary = eval(d.name()) # TODO(stevan): can we just keep the str here?
                faults.append(dictionary)
        faults = sorted(faults, key=order)
        event.faults = json.dumps({"faults": faults})

        return event

def main():
    config = create_config()
    storage = SqliteStorage()

    previous_faults = storage.load_previous_faults(config)
    potential_faults = storage.load_potential_faults(config)

    formula = create_sat_formula(config, previous_faults, potential_faults)

    (result, model, statistics) = sat_solve(formula)

    event = create_log_event(config, result, model, statistics)
    storage.store(event)
    logging.debug(event.faults)
    print(event.faults)

if __name__ == '__main__':
    main()
