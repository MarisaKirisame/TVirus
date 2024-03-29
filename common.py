import os
import json
import datetime

class Result:
    def __init__(self, path, log, config, ok):
        self.path = path
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

def get_time():
    return datetime.datetime.now().strftime("%m%d_%H%M%S")

def write_to(path, val):
    with open(path, "a") as f:
        f.write(val)

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
    return Result(path, result, config, ok)

def get_spacetime(r, use_total_allocated=False):
    if len(r.log) == 0:
        return (None, None)
    start_time = r.log[0]["timestamp"]
    space = 0
    time = None
    for l in r.log:
        if l["name"] == "mem":
            if use_total_allocated:
                space = max(space, l["total_allocated"])
            else:
                space = max(space, l["allocated"])
        time = l["timestamp"] - start_time
    return (space, time)
