from common import *

import json
import matplotlib.pyplot as plt
import os
import sys
import numpy as np

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
        print(r.config)
        if r.config["backend"]["name"] == "baseline":
            baseline_space, baseline_time = get_spacetime(r)

    zombie_points = []
    baseline_points = []

    for r in runs:
        space, time = get_spacetime(r)
        print(f"space = {space/1e6}MB, time = {time/1e9}s")
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

    x, y = zip(*zombie_points)
    plt.scatter(x, y, label="zombie")

    coef = np.polyfit(x, [1/v for v in y], 1)
    poly1d_fn = np.poly1d(coef)

    plt.plot(sorted(x), 1/poly1d_fn(sorted(x)), '--k') #'--k'=black dashed line, 'yo' = yellow circle marker

    x, y = zip(*baseline_points)
    plt.scatter(x, y, label="baseline")

    plt.xscale('log')
    plt.xlabel("space")

    plt.yscale('log')
    plt.ylabel("time")

    plt.legend()
    plt.savefig("./result.png")
    plt.title(res.config["program"])
    #plt.show()
    plt.close()

if len(sys.argv) > 1:
    d = sys.argv[1]
else:
    d = "log/" + sorted(os.listdir("log"))[-1]

for x in os.listdir(d):
    if os.path.isdir(f"{d}/{x}"):
        work(f"{d}/{x}")

zombie_aggregate = []
for k in aggregate:
    baseline_points, zombie_points = aggregate[k]

    zombie_aggregate += zombie_points

    x, y = zip(*zombie_points)
    plt.scatter(x, y, label="zombie_" + k)

    x, y = zip(*baseline_points)
    plt.scatter(x, y, label="baseline")

x, y = zip(*zombie_aggregate)

coef = np.polyfit(x, [1/v for v in y], 1)
poly1d_fn = np.poly1d(coef)

plt.plot(sorted(x), 1/poly1d_fn(sorted(x)), '--k') #'--k'=black dashed line, 'yo' = yellow circle marker

plt.xscale('log')
plt.xlabel("space")

plt.yscale('log')
plt.ylabel("time")

plt.legend()
plt.savefig("./result.png")
plt.show()
plt.close()
