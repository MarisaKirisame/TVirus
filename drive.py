from EVAL import *
from common import *

import os
import time
import subprocess

zombie_backend = {
  "name": "zombie",
  "limit": NONDET(
    {"name": "no"},
    {"name": "RelToZombie",
     "ratio": NONDET(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005, 0.0002, 0.0001)})}

zombie_backend = {
  "name": "zombie",
  "limit": NONDET(
    {"name": "no"},
    {"name": "RelToZombie",
     "ratio": NONDET(0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001)})}

zombie_noevict_backend = {
  "name": "zombie",
  "limit": NONDET({"name": "no"})}

baseline_backend = {
  "name": "baseline",
  "limit": {"name": "no"}
}

default = {
  "program": NONDET("taba", "rbt", "pascal"),
  "backend": QUOTE(NONDET(baseline_backend, zombie_backend))
}

def run(path, ev):
    os.mkdir(path)
    for x in flatten_nondet(ev).l:
        subpath = path + "/" + get_time()
        os.mkdir(subpath)
        sq = strip_quote(x)
        write_to(subpath + "/config", str(sq))
        cmd = f"""python3 inner_drive.py {subpath} {repr(str(sq))} >> {path}/output 2>&1"""
        print(f"running {cmd}")
        subprocess.run(cmd, shell=True, check=False)
        time.sleep(2)

run("log/" + get_time(), default)
