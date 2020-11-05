#!/usr/bin/env python3

import argparse
import os
import sqlite3

parser = argparse.ArgumentParser(description='Visualise network trace of run from a test.')
parser.add_argument('--test-id', metavar='TEST_ID', type=int, required=True,
                    help='the test id')
parser.add_argument('--run-id', metavar='RUN_ID', type=int, required=True,
                    help='the run id')
args = parser.parse_args()

db = os.path.abspath(os.path.join(os.path.dirname(__file__),
                                  '..', 'db', 'detsys.sqlite3'))

conn = sqlite3.connect(db)
conn.row_factory = sqlite3.Row
c = conn.cursor()

c.execute("select * from network_trace where test_id = (?) and run_id = (?)",
          (args.test_id, args.run_id))

nodes = set()
edges = set()
max_time = 0

for r in c:
    nodes.add(r['from'])
    nodes.add(r['to'])

    edges.add((r['from'], r['to'], r['sent_logical_time'], r['at']))
    max_time = max(max_time, r['at'])
nodes = list(nodes)

dot = """
digraph network_trace {
  rankdir=TD
  splines=polyline
"""

dot += """
  subgraph cluster_proc_nodes {
    label="";
"""

for id, n in enumerate(nodes):
    dot += "    proc_%s [label=\"%s\", group=\"%s\"]\n" % (id, n, n)

dot +="  }\n"

def occurs(n, i):
    for fro, to, st, at in edges:
        if (fro == n and st == i) or (to == n and i == at):
            return True
    return False

for id, n in enumerate(nodes):
    for i in range(0, max_time+1):
        extra = "" if occurs(n, i) else ", shape=\"point\""
        dot += "  node_%s_%d[label=\"%s\", group=\"%s\"%s];\n" % (id, i, i, n, extra)
    dot += "\n"

for f,t,st,at in edges:
    fi = nodes.index(f)
    ti = nodes.index(t)
    dot += "  node_%s_%d -> node_%s_%d [constraint=false];\n" % (fi, st, ti, at)

dot += "\n"
for id, n in enumerate(nodes):
    dot+= "  edge[weight=2, arrowhead=none, color=gray75, fillcolor=gray75];\n"
    dot+= "  proc_%s" % id
    for i in range(0, max_time+1):
        dot += " -> node_%s_%d" % (id, i)
    dot+=";\n"

dot +="}"

print (dot)
