import unittest
from ldfi import order

class TestSortedFaults(unittest.TestCase):
    def test_sorted_faults(self):
        l = sorted([{"kind": "omission", "from": "frontend", "to": "register2", "at": 2},
                    {"kind": "crash", "from": "frontend", "at": 1},
                    {"kind": "crash", "from": "frontend", "at": 0},
                    {"kind": "omission", "from": "frontend", "to": "register2", "at": 1}],
                   key=order)
        assert(l ==
               [{"kind": "crash", "from": "frontend", "at": 0},
                {"kind": "crash", "from": "frontend", "at": 1},
                {"kind": "omission", "from": "frontend", "to": "register2", "at": 1},
                {"kind": "omission", "from": "frontend", "to": "register2", "at": 2}])

if __name__ == '__main__':
    unittest.main()
