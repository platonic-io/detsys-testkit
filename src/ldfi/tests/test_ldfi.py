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

def o(f, t, at):
    return ('{"kind": "omission", "from": "%s", "to": "%s", "at": %d}' % (f, t, at))

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

    faults1 = '{"faults": [%s]}' % o("A", "B", 1)
    faults2 = '{"faults": [%s, %s]}' % (o("A", "B", 1), o("A", "C", 2))
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
                           test_id  INT  NOT NULL,
                           run_id   INT  NOT NULL,
                           kind     TEXT NOT NULL,
                           `from`   TEXT NOT NULL,
                           `to`     TEXT NOT NULL,
                           at       INT  NOT NULL)""")
    storage.conn.commit()
    config = ldfi.Config(1, [0, 1], 2, 0)
    assert storage.load_potential_faults(config) == [[], []]

    storage.c.execute("INSERT INTO network_trace VALUES(?, ?, ?, ?, ?, ?)",
                      (1, 0, "message", "A", "B", 1))
    storage.c.execute("INSERT INTO network_trace VALUES(?, ?, ?, ?, ?, ?)",
                      (1, 0, "message", "A", "C", 1))
    storage.c.execute("INSERT INTO network_trace VALUES(?, ?, ?, ?, ?, ?)",
                      (1, 1, "message", "A", "C", 2))
    storage.conn.commit()
    assert storage.load_potential_faults(config) == [
        [{"from": "A", "to": "B", "at": 1},
         {"from": "A", "to": "C", "at": 1}],
        [{"from": "A", "to": "C", "at": 2}]]

def test_create_formula(caplog):
    caplog.set_level(logging.DEBUG)

    config = ldfi.Config(-1, [-1], 2, 0)
    oab1 = o("A", "B", 1)
    oac1 = o("A", "C", 1)
    ab1 = z3.Bool(oab1)
    ac1 = z3.Bool(oac1)

    crashes = set()

    # First run
    previous_faults = [[]]
    potential_faults = [[oab1, oac1]]
    data = ldfi.Data(previous_faults, potential_faults, crashes)
    ldfi.sanity_check(data)
    formula = ldfi.create_sat_formula(config, data)
    assert(formula == And(Or(ab1, ac1)))
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.sat
    event = ldfi.create_log_event(config, result, model, statistics)
    assert event.faults == ('{"faults": [%s]}' % oab1)

    # Second run
    previous_faults = [[oab1]]
    potential_faults = [[oab1, oac1], [oac1]]
    data = ldfi.Data(previous_faults, potential_faults, crashes)
    ldfi.sanity_check(data)
    formula = ldfi.create_sat_formula(config, data)
    expected_formula = And(Or(ab1, ac1),
                           Or(Or(ac1),
                              Not(And(ab1))))
    assert(formula == expected_formula)
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.sat
    event = ldfi.create_log_event(config, result, model, statistics)
    assert event.faults == ('{"faults": [%s]}' % oac1)

    # Third run
    previous_faults = [[oab1], [oac1]]
    potential_faults = [[oab1, oac1], [oac1], [oab1]]
    data = ldfi.Data(previous_faults, potential_faults, crashes)
    ldfi.sanity_check(data)
    formula = ldfi.create_sat_formula(config, data)
    expected_formula = And(Or(ab1, ac1),
                           Or(Or(ac1),
                              Not(And(ab1))),
                           Or(Or(ab1),
                              Not(And(ac1))))
    assert(formula == expected_formula)
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.sat
    event = ldfi.create_log_event(config, result, model, statistics)
    assert event.faults == ('{"faults": [%s, %s]}' % (oab1, oac1))

    # Forth run
    oab2 = o("A", "B", 2) # newly discovered edge
    ab2 = z3.Bool(oab2)
    previous_faults = [[oab1], [oac1], [oab1, oac1]]
    potential_faults = [[oab1, oac1], [oac1], [oab1], [oab2]] # solver finds: oab2
    data = ldfi.Data(previous_faults, potential_faults, crashes)
    ldfi.sanity_check(data)
    formula = ldfi.create_sat_formula(config, data)
    expected_formula = And(Or(ab1, ac1),
                           Or(Or(ac1),
                              Not(And(ab1))),
                           Or(Or(ab1),
                              Not(And(ac1))),
                           Or(Or(ab2),
                              Not(And(ab1, ac1))))
    (result, model, statistics) = ldfi.sat_solve(formula)
    assert result == z3.sat
    event = ldfi.create_log_event(config, result, model, statistics)
    assert event.faults == ('{"faults": [%s, %s, %s]}' % (oab1, oab2, oac1))

    # Fifth run
    previous_faults = [[oab1], [oac1], [oab1, oac1], [oab2]]
    potential_faults = [[oab1, oac1], [oac1], [oab1], [oab2], []] # done
    data = ldfi.Data(previous_faults, potential_faults, crashes)
    ldfi.sanity_check(data)
    formula = ldfi.create_sat_formula(config, data)
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
