from common import *

import json
import matplotlib.pyplot as plt
import os
import sys
import numpy as np
import math
import dominate
from dominate.tags import *
import subprocess

out_path = "output/" + get_time()
os.makedirs(out_path)

aggregate = {}

def plot(result):
    start_time = None
    x = []
    allocated = []
    total_allocated = []
    for r in result.log:
        if start_time is None:
            start_time = r["timestamp"]
        x.append((r["timestamp"] - start_time) / 1e9)
        allocated.append(r["allocated"] / 1e6)
        total_allocated.append(r["total_allocated"] / 1e6)
    plt.plot(x, allocated, label=result.config["backend"]["name"])
    #plt.plot(x, total_allocated, label=result.config["backend"]["name"]+"_total")
    plt.xlabel("time (seconds)")
    plt.ylabel("space (MB)")

def lr(points):
    if len(points) > 0:
        points.sort()
        x, y = zip(*points)

        coef = np.polyfit([math.log(x_) for x_ in x], [math.log(y_) for y_ in y], 1)
        poly1d_fn = np.poly1d(coef)

        plt.scatter(1, 1, label="baseline")
        plt.plot(x, [math.exp(y_) for y_ in poly1d_fn([math.log(x_) for x_ in x])], '--k') #'--k'=black dashed line, 'yo' = yellow circle marker

        p(f"10x space = {round(math.exp(poly1d_fn(math.log(1)) - poly1d_fn(math.log(10))), 3)}x time!")

COUNTER = 0
def count():
    global COUNTER
    ret = COUNTER
    COUNTER += 1
    return ret

def flush_plt():
    rel_path = str(count()) + ".png"
    plt.savefig(f"{out_path}/{rel_path}")
    plt.close()
    img(src=rel_path)
    br()

def work(path):
    runs = []

    for x in os.listdir(path):
        if os.path.isdir(f"{path}/{x}"):
            res = read(f"{path}/{x}")
            if res.ok:
                runs.append(res)

    baseline_time = None
    baseline_space = None

    for r in runs:
        if r.config["backend"]["name"] == "baseline":
            baseline_space, baseline_time = get_spacetime(r)

    zombie_points = []
    baseline_points = []

    doc = dominate.document(title='Dominate your HTML')
    with doc:
        for r in runs:
            space, time = get_spacetime(r)
            p(f"space = {space/1e6}MB, time = {time/1e9}s")
            space = space / baseline_space
            time = time / baseline_time
            if r.config["backend"]["name"] == "zombie":
                zombie_points.append((space, time))
            else:
                baseline_points.append((space, time))
                #space, time = get_spacetime(r, use_total_allocated=True)
                #print(f"space = {space/1e6}MB, time = {time/1e9}s")
                #space = space / zombie_baseline_space
                #time = time / zombie_baseline_time
                #baseline_points.append((space, time))

        assert res.config["program"] not in aggregate
        aggregate[res.config["program"]] = (baseline_points, zombie_points)

        if len(zombie_points) != 0:
            x, y = zip(*zombie_points)
            plt.scatter(x, y, label="zombie")

            lr(zombie_points)

        if len(baseline_points) != 0:
            x, y = zip(*baseline_points)
            plt.scatter(x, y, label="baseline")

        plt.xscale('log')
        plt.xlabel("space")

        plt.yscale('log')
        plt.ylabel("time")

        plt.legend()
        plt.title(res.config["program"])
        flush_plt()

    return (res.config["program"], doc)

if len(sys.argv) > 1:
    d = sys.argv[1]
else:
    d = "log/" + sorted(os.listdir("log"))[-1]

subpages = []
for x in os.listdir(d):
    if os.path.isdir(f"{d}/{x}"):
        subpages.append(work(f"{d}/{x}"))

zombie_aggregate = []
for k in aggregate:
    baseline_points, zombie_points = aggregate[k]

    zombie_aggregate += zombie_points

    if len(zombie_points) != 0:
        x, y = zip(*zombie_points)
        plt.scatter(x, y, label="zombie_" + k)

    #if len(baseline_points) != 0:
    #    x, y = zip(*baseline_points)
    #    plt.scatter(x, y, label="baseline")

doc = dominate.document(title='Dominate your HTML')

with doc:
    lr(zombie_aggregate)

    plt.xscale('log')
    plt.xlabel("space")

    plt.yscale('log')
    plt.ylabel("time")

    plt.legend()
    flush_plt()

    for program_name, page in subpages:
        rel_path = str(count()) + ".html"
        write_to(f"{out_path}/{rel_path}", str(page))
        a(program_name, href=rel_path)
        br()

write_to(out_path + "/index.html", str(doc))
subprocess.run(f"xdg-open {out_path}/index.html", shell=True, check=True)
