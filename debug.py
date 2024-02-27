from common import *

import json
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import os
import sys
import numpy as np
import math
import dominate
from dominate.tags import *
import subprocess
import random
import pydot
import io
import itertools

def is_sorted(iterable, compare=lambda a, b: a <= b):
    return all(compare(a, b) for a, b in itertools.pairwise(iterable))

out_dir = get_time()
out_path = "output/" + out_dir
os.makedirs(out_path)

COUNTER = 0
def count():
    global COUNTER
    ret = COUNTER
    COUNTER += 1
    return ret

d = "log/" + sorted(os.listdir("log"))[-1]

def get_subdir(path):
    return [path + "/" + x for x in os.listdir(path) if os.path.isdir(path + "/" + x)]

def singleton(x):
    assert len(x) == 1
    return x[0]

final_dir = singleton(get_subdir(singleton(get_subdir(d))))
r = read(final_dir)

insert_context = []
evict = []
distinct_evict = set()

for l in r.log:
    if l["name"] == "insert_context":
        insert_context.append(l)
    elif l["name"] == "evict":
        evict.append(l)
        distinct_evict.add(l["t"])

#for e in evict:
#    print(e)
#raise

def draw(limit):
    inserted = set()
    nodes = []
    edges = set() # avoid duplicating edges
    evicted = set()

    for e in evict:
        if len(evicted) < limit:
            evicted.add(e["t"])

    graph = pydot.Dot("my_graph")

    for ic in insert_context:
        print(ic)
        if ic["t"] not in inserted:
            nodes.append(ic)
            inserted.add(ic["t"])

    print(len(insert_context))
    print(len(nodes))
    print(len(evicted))

    for n in nodes:
        if n["t"] != 1:
            color = "black" if n["t"] in evicted else "gold"
            graph.add_node(pydot.Node(str(n["t"]), color=color))

    assert is_sorted(nodes, compare=lambda a, b: a["t"] <= b["t"])

    def get_context(t):
        for i in range(len(nodes)):
            assert nodes[i]["t"] != t
            if nodes[i]["t"] < t and nodes[i + 1]["t"] > t:
                return nodes[i]["t"]

    def get_parent(t):
        for i in range(len(nodes)):
            if nodes[i+1]["t"] == t:
                return nodes[i]["t"]

    def add_edge(l, r):
        if r != 1 and (l, r) not in edges:
            edges.add((l, r))
            graph.add_edge(pydot.Edge(str(l), str(r), color="blue"))

    for n in nodes:
        for d in n["deps"]:
            add_edge(n["t"], get_context(d))
        if n["t"] != 1:
            add_edge(n["t"], get_parent(n["t"]))

    graph.write_svg(f"{out_path}/{count()}.svg")

print(evict)
for e in evict:
    print(e)
#draw(100)
for i in range(len(distinct_evict)):
    draw(i)
