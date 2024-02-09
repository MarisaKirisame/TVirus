import json
import matplotlib.pyplot as plt
import os

class Result:
    def __init__(self, log, config, ok):
        self.log = log
        self.config = config
        self.ok = ok

    def __str__(self):
        return f"Result({self.config})"

def try_read_file(path):
    if os.path.isfile(path):
        return read_file(path)
    else:
        return ""

def read_file(path):
    with open(path, "r") as f:
        return f.read()

def readlines_file(path):
    with open(path, "r") as f:
        return f.readlines()

def read(path):
    result = []
    def failed():
        print(f"{path} failed")
        print(''.join(readlines_file(f"{path}/output")[-10:]))
    if try_read_file(f"{path}/returncode") == "0":
        with open(f"{path}/log", "r") as f:
            for line in f:
                result.append(json.loads(line))
        ok = True
    else:
        failed()
        ok = False
        print(try_read_file(f"{path}/returncode"))
    with open(f"{path}/config", "r") as f:
        lines = f.readlines()
        assert len(lines) == 1
        config = eval(lines[0])
    return Result(result, config, ok)

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

d = "log/" + sorted(os.listdir("log"))[-1]

runs = []

for x in os.listdir(d):
    res = read(f"{d}/{x}")
    if res.ok:
        runs.append(res)

zombie_baseline_time = None
zombie_baseline_space = None

def get_spacetime(r, use_total_allocated=False):
    start_time = None
    space = None
    time = None
    for l in r.log:
        if start_time is None:
            space = 0
            start_time = l["timestamp"]
        if use_total_allocated:
            space = max(space, l["total_allocated"])
        else:
            space = max(space, l["allocated"])
        time = l["timestamp"] - start_time
    return (space, time)

for r in runs:
    print(r.config)
    if r.config["backend"]["name"] == "zombie" and r.config["backend"]["limit"]["name"] == "no":
        zombie_baseline_space, zombie_baseline_time = get_spacetime(r)

zombie_points = []
baseline_points = []

for r in runs:
    space, time = get_spacetime(r)
    print(f"space = {space/1e6}MB, time = {time/1e9}s")
    space = space / zombie_baseline_space
    time = time / zombie_baseline_time
    if r.config["backend"]["name"] == "zombie":
        zombie_points.append((space, time))
    else:
        baseline_points.append((space, time))
        #space, time = get_spacetime(r, use_total_allocated=True)
        #print(f"space = {space/1e6}MB, time = {time/1e9}s")
        #space = space / zombie_baseline_space
        #time = time / zombie_baseline_time
        #baseline_points.append((space, time))

x, y = zip(*zombie_points)
plt.scatter(x, y, label="zombie")

x, y = zip(*baseline_points)
plt.scatter(x, y, label="baseline")

plt.xscale('log')
plt.xlabel("space")

plt.yscale('log')
plt.ylabel("time")

plt.legend()
plt.savefig("./result.png")
plt.title(res.config["program"])
plt.show()
