from EVAL import *
import datetime
import os
import time
import subprocess

zombie_backend = {
  "name": "zombie",
  "limit": NONDET(
    {"name": "no"},
    {"name": "RelToZombie",
     "ratio": NONDET(0.5, 0.3, 0.2, 0.1, 0.05, 0.03, 0.02, 0.01, 0.005, 0.003, 0.002, 0.001)})}

zombie_backend = {
  "name": "zombie",
  "limit": NONDET(
    {"name": "no"},
    {"name": "RelToZombie",
     "ratio": NONDET(0.1, 0.01)})}

zombie_noevict_backend = {
  "name": "zombie",
  "limit": NONDET({"name": "no"})}

baseline_backend = {"name": "baseline",
     "limit": {"name": "no"}}

default = {
  "program": "taba",
  "backend": NONDET(baseline_backend, zombie_backend)
}

def read_file(path):
    with open(path, "r") as f:
        return f.read()

def get_time():
    return datetime.datetime.now().strftime("%m%d_%H%M%S")

def write_to(path, val):
    with open(path, "a") as f:
        f.write(val)

zombie_memory_limit = None

def call(x):
    global zombie_memory_limit
    this_eval_dir = eval_dir + "/" + get_time()
    os.mkdir(this_eval_dir)
    write_to(this_eval_dir + "/config", str(x))
    backend = x['backend']
    limit = backend['limit']
    tv_backend = backend['name']
    if limit['name'] == "no":
        tv_limit = 0
    else:
        assert(limit['name'] == "RelToZombie")
        tv_limit = int(zombie_memory_limit * limit['ratio'])
    cmd = f"""mill codegen {x['program']} {tv_backend} {tv_limit} {this_eval_dir}/log"""
    print(f"running: {cmd}")

    subprocess.run(f"{cmd} >> {this_eval_dir}/output 2>&1", shell=True, check=False)
    subprocess.run(f"mv output.cpp {this_eval_dir}/output.cpp", shell=True, check=False)
    output = subprocess.run(f"./output >> {this_eval_dir}/output 2>&1", shell=True, check=False)
    write_to(f"{this_eval_dir}/returncode", str(output.returncode))
    subprocess.run(f"mv output {this_eval_dir}/program", shell=True, check=False)

    if tv_backend == "zombie" and limit['name'] == "no":
        retcode = read_file(f"{this_eval_dir}/returncode")
        if retcode != "0":
            subprocess.run(f"cat {this_eval_dir}/output", shell=True, check=False)
            raise
        else:
            with open(f"{this_eval_dir}/log") as f:
                for l in f.readlines():
                    allocated = eval(l)["allocated"]
                    if zombie_memory_limit is None or zombie_memory_limit < allocated:
                        zombie_memory_limit = allocated
            assert(zombie_memory_limit is not None)

def run(ev):
    for x in flatten_nondet(ev).l:
        call(x)

eval_dir = "log/" + get_time()
os.mkdir(eval_dir)

run(default)
