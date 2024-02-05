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
for x in os.listdir(d):
    res = read(f"{d}/{x}")
    if res.ok:
        plot(res)

plt.legend()
plt.savefig("./result.png")
plt.title(res.config["program"])
plt.show()
