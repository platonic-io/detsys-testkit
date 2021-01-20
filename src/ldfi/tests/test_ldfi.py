import json
import ldfi
import tempfile
import logging
import os
import sqlite3
import z3
from z3 import (And, Or, Not, Bool)

def test_sorted_faults():
    l = sorted([{"kind": "omission", "from": "frontend", "to": "register2", "at": 2},
                {"kind": "crash", "from": "frontend", "at": 1},
                {"kind": "crash", "from": "frontend", "at": 0},
                {"kind": "omission", "from": "frontend", "to": "register2", "at": 1}],
               key=ldfi.order)
    assert(l ==
           [{"kind": "crash", "from": "frontend", "at": 0},
            {"kind": "crash", "from": "frontend", "at": 1},
            {"kind": "omission", "from": "frontend", "to": "register2", "at": 1},
            {"kind": "omission", "from": "frontend", "to": "register2", "at": 2}])

def test_load_previous_faults():
    db = os.path.join(tempfile.gettempdir(), "detsys_pytest.sqlite3")
    os.environ["DETSYS_DB"] = db
    storage = ldfi.SqliteStorage()
    # TODO(stevan): add contract test in db component saying ldfi expects the
    # following table and fields.
    storage.c.execute("""CREATE TABLE IF NOT EXISTS faults (
                           test_id  INT  NOT NULL,
                           run_id   INT  NOT NULL,
                           faults   JSON NOT NULL)""")
    storage.conn.commit()
    config = ldfi.Config(1, [0, 1], 2, 0)
    assert storage.load_previous_faults(config) == []

    faults1 = json.dumps({"faults": [o("A", "B", 1)]})
    faults2 = json.dumps({"faults": [o("A", "B", 1), o("A", "C", 2)]})
    storage.c.execute("INSERT INTO faults VALUES(?, ?, ?)", (1, 0, faults1))
    storage.c.execute("INSERT INTO faults VALUES(?, ?, ?)", (1, 1, faults2))
    storage.conn.commit()
    assert storage.load_previous_faults(config) == [
        {"faults": [{"kind": "omission", "from": "A", "to": "B", "at": 1}]},
        {"faults": [{"kind": "omission", "from": "A", "to": "B", "at": 1},
                    {"kind": "omission", "from": "A", "to": "C", "at": 2}]}]

def test_load_potential_faults(caplog):
    caplog.set_level(logging.DEBUG)
    db = os.path.join(tempfile.gettempdir(), "detsys_pytest.sqlite3")
    os.environ["DETSYS_DB"] = db
    storage = ldfi.SqliteStorage()
    # TODO(stevan): add contract test in db component saying ldfi expects the
    # following table and fields.
    storage.c.execute("""CREATE TABLE IF NOT EXISTS network_trace (
                           test_id           INT  NOT NULL,
                           run_id            INT  NOT NULL,
                           kind              TEXT NOT NULL,
                           `from`            TEXT NOT NULL,
                           `to`              TEXT NOT NULL,
                           at                INT  NOT NULL,
                           sent_logical_time INT  NOT NULL)""")
    storage.conn.commit()
    config = ldfi.Config(1, [0, 1], 2, 0)
    assert storage.load_potential_faults(config) == [[], []]

    storage.c.execute("INSERT INTO network_trace VALUES(?, ?, ?, ?, ?, ?, ?)",
                      (1, 0, "message", "A", "B", 1, 1))
    storage.c.execute("INSERT INTO network_trace VALUES(?, ?, ?, ?, ?, ?, ?)",
                      (1, 0, "message", "A", "C", 1, 1))
    storage.c.execute("INSERT INTO network_trace VALUES(?, ?, ?, ?, ?, ?, ?)",
                      (1, 1, "message", "A", "C", 2, 3))
    storage.conn.commit()
    assert storage.load_potential_faults(config) == [
        [{"from": "A", "to": "B", "at": 1, "sent_logical_time": 1},
         {"from": "A", "to": "C", "at": 1, "sent_logical_time": 1}],
        [{"from": "A", "to": "C", "at": 2, "sent_logical_time": 3}]]

def o(f, t, at):
    return {"kind": "omission", "from": f, "to": t, "at": at}

def msg(f, t, at):
    return {"from": f, "to": t, "at": at, "sent_logical_time": at - 1}

def test_create_formula2(caplog):
    caplog.set_level(logging.DEBUG)

    config = ldfi.Config(-1, [-1], 3, 0)
    oab1 = o("A", "B", 1)
    oac1 = o("A", "C", 1)
    ab1 = z3.Bool(str(oab1))
    ac1 = z3.Bool(str(oac1))

    # First run
    previous_faults = []
    potential_faults = [[msg("A", "B", 1), msg("A", "C", 1)]]
    formula = ldfi.create_sat_formula(config, previous_faults, potential_faults)
    assert formula == And(Or(ab1, ac1))
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.sat
    event = ldfi.create_log_event(config, result, model, statistics)
    expected_faults = json.dumps({"faults": [oab1]})
    assert event.faults == expected_faults

    # Second run
    previous_faults.append([oab1])
    potential_faults.append([msg("A", "C", 1)])
    formula = ldfi.create_sat_formula(config, previous_faults, potential_faults)
    expected_formula = And(Or(ab1, ac1),
                           Or(Or(ac1),
                              Not(And(ab1))))
    assert formula == expected_formula
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.sat
    event = ldfi.create_log_event(config, result, model, statistics)
    expected_faults = json.dumps({"faults": [oac1]})
    assert event.faults == expected_faults

    # Third run
    previous_faults.append([oac1])
    potential_faults.append([msg("A", "B", 1)])
    formula = ldfi.create_sat_formula(config, previous_faults, potential_faults)
    expected_formula = And(Or(ab1, ac1),
                           Or(Or(ac1),
                              Not(And(ab1))),
                           Or(Or(ab1),
                              Not(And(ac1))))
    assert formula == expected_formula
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.sat
    event = ldfi.create_log_event(config, result, model, statistics)
    expected_faults = json.dumps({"faults": [oab1, oac1]})
    assert event.faults == expected_faults

    # Forth run
    oab2 = o("A", "B", 2) # newly discovered edge
    oac3 = o("A", "C", 3) # also new, but not less than EFF...
    ab2 = z3.Bool(str(oab2))
    ac3 = z3.Bool(str(oac3))
    previous_faults.append([oab1, oac1])
    potential_faults.append([msg("A", "B", 2), msg("A", "C", 3)])
    formula = ldfi.create_sat_formula(config, previous_faults, potential_faults)
    expected_formula = And(Or(ab1, ac1),
                           Or(Or(ac1),
                              Not(And(ab1))),
                           Or(Or(ab1),
                              Not(And(ac1))),
                           Or(Or(ab2), # Note that ac3 is not here because EFF = 3.
                              Not(And(ab1, ac1))))
    assert formula == expected_formula
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.sat
    event = ldfi.create_log_event(config, result, model, statistics)
    expected_faults = json.dumps({"faults": [oab1, oab2, oac1]})
    assert event.faults == expected_faults

    # Fifth run
    previous_faults.append([oab1, oab2, oac1])
    potential_faults.append([])
    formula = ldfi.create_sat_formula(config, previous_faults, potential_faults)
    expected_formula = And(Or(ab1, ac1),
                           Or(Or(ac1),
                              Not(And(ab1))),
                           Or(Or(ab1),
                              Not(And(ac1))),
                           Or(Or(ab2),
                              Not(And(ab1, ac1))),
                           Or(Not(And(ab1, ab2, ac1))))
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.unsat
