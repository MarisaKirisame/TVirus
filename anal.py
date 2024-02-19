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

out_dir = get_time()
out_path = "output/" + out_dir
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

def grab_file(path):
    new_path = str(count()) + ".txt"
    subprocess.run(f"cp {path} {out_path}/{new_path}", shell=True, check=True)
    a(os.path.split(path)[1], href=new_path)

def flush_plt():
    rel_path = str(count()) + ".png"
    plt.legend()
    plt.savefig(f"{out_path}/{rel_path}")
    plt.close()
    img(src=rel_path)
    br()

class make_doc(dominate.document):
    def _add_to_ctx(self): pass # don't add to contexts

def doc_to_path(d):
    rel_path = str(count()) + ".html"
    write_to(f"{out_path}/{rel_path}", str(d))
    return rel_path

def individual_run(r):
    doc = make_doc(title=r.path)
    with doc:
        grab_file(r.path + "/output")
        largest_uf = []
        replay = []
        insert_context = []
        current_tock_change = []
        begin_timestamp = None

        for l in r.log:
            if begin_timestamp is None:
                begin_timestamp = l["timestamp"]
            if l["name"] == "largest_uf":
                largest_uf.append(l)
            if l["name"] == "replay_begin" or l["name"] == "replay_end":
                replay.append(l)
            if l["name"] == "insert_context":
                insert_context.append(l)
            if l["name"] == "current_tock_change":
                current_tock_change.append(l)

        if len(largest_uf) > 0:
            plt.plot([(l["timestamp"] - begin_timestamp) / 1e9 for l in largest_uf], [l["total_uf_root_count"] for l in largest_uf], label="uf root count")
            plt.plot([(l["timestamp"] - begin_timestamp) / 1e9 for l in largest_uf], [l["total_uf_node_count"] for l in largest_uf], label="uf node count")
            plt.xlabel("time")
            plt.ylim(ymin=0)
            flush_plt()

            plt.plot([(l["timestamp"] - begin_timestamp) / 1e9 for l in largest_uf], [l["value"] for l in largest_uf], label="largest uf")
            plt.xlabel("time")
            plt.ylim(ymin=0)
            flush_plt()

            plt.plot([(l["timestamp"] - begin_timestamp) / 1e9 for l in largest_uf], [l["value"] for l in largest_uf], label="largest uf")
            plt.xlabel("time")
            plt.yscale('log')
            plt.ylim(ymin=1)
            flush_plt()

        if len(replay) > 0:
            plt.plot([(l["timestamp"] - begin_timestamp) / 1e9 for l in replay], [l["depth"] for l in replay], label="replay depth")
            plt.xlabel("time")
            flush_plt()

        if len(insert_context) > 0:
            plt.plot([(l["timestamp"] - begin_timestamp) / 1e9 for l in insert_context], [l["time_taken"] for l in insert_context], label="individual context time taken")
            plt.xlabel("time")
            flush_plt()

            plt.plot([(l["timestamp"] - begin_timestamp) / 1e9 for l in insert_context], [l["splaylist_size"] for l in insert_context], label="splaylist size")
            plt.xlabel("time")
            flush_plt()

        if len(current_tock_change) > 0:
            plt.plot([(l["timestamp"] - begin_timestamp) / 1e9 for l in current_tock_change], [l["value"] for l in current_tock_change], label="current_tock")
            plt.xlabel("time")
            flush_plt()

    return doc_to_path(doc)

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

    doc = make_doc(title=path)
    with doc:
        for r in runs:
            space, time = get_spacetime(r)
            a(f"space = {space/1e6}MB, time = {time/1e9}s", href=individual_run(r))
            br()
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

        plt.title(res.config["program"])
        flush_plt()

    return (res.config["program"], doc_to_path(doc))

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

doc = make_doc(title=d)

with doc:
    lr(zombie_aggregate)

    plt.xscale('log')
    plt.xlabel("space")

    plt.yscale('log')
    plt.ylabel("time")

    flush_plt()

    for program_name, page_path in subpages:
        a(program_name, href=page_path)
        br()

    grab_file(f"{d}/output")

write_to(out_path + "/index.html", str(doc))

if subprocess.run("command -v nightly-results", shell=True).returncode == 0:
    subprocess.run(f"""nightly-results publish {out_path}""", shell=True, check=True)
else:
    subprocess.run(f"xdg-open {out_path}/index.html", shell=True, check=True)
