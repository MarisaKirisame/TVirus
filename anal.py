import json
import matplotlib.pyplot as plt
import os

class Result:
    def __init__(self, log, config):
        self.log = log
        self.config = config

    def __str__(self):
        return f"Result({self.config})"

def read(path):
    result = []
    with open(f"{path}/log", "r") as f:
        for line in f:
            result.append(json.loads(line))
    with open(f"{path}/config", "r") as f:
        lines = f.readlines()
        assert len(lines) == 1
        config = eval(lines[0])
    return Result(result, config)

def plot(result):
    start_time = None
    x = []
    y = []
    for r in result.log:
        if start_time is None:
            start_time = r["timestamp"]
        x.append((r["timestamp"] - start_time) / 1e9)
        y.append(r["allocated"] / 1e6)
    plt.plot(x, y, label=result.config["backend"])
    plt.xlabel("time (seconds)")
    plt.ylabel("space (MB)")

d = "log/" + sorted(os.listdir("log"))[-1]
for x in os.listdir(d):
    res = read(f"{d}/{x}")
    plot(res)

plt.legend()
plt.show()
