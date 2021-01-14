import unittest
import ldfi
import z3
from z3 import (And, Or, Not, Bool)

class TestSortedFaults(unittest.TestCase):
    def test_sorted_faults(self):
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

class TestCreateFormula(unittest.TestCase):
    def test_create_formula(self):
        config = ldfi.Config(-1, [-1], 2, 0)
        previous_faults = [[]]
        oab1 = o("A", "B", 1)
        oac1 = o("A", "C", 1)

        # First run
        potential_faults = [[oab1, oac1]]
        crashes = set()
        data = ldfi.Data(previous_faults, potential_faults, crashes)
        ldfi.sanity_check(data)
        formula = ldfi.create_sat_formula(config, data)
        assert(formula == And(Or(Bool(oab1), Bool(oac1))))
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
        print(formula)
        assert(formula == And(Or(Or(Bool(oab1), Bool(oac1)),
                                 Not(And(Bool(oab1)))),
                              Or(Or(Bool(oac1)),
                                 Not(And(Bool(oab1))))))

if __name__ == '__main__':
    unittest.main()
