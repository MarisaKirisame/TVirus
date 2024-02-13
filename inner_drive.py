import sys
import os
import time
import subprocess

from EVAL import *
from common import *

zombie_memory_limit = None
zombie_runtime = None

path = sys.argv[1]
ev = eval(sys.argv[2])

def call(x):
    global zombie_memory_limit
    global zombie_runtime
    this_eval_dir = path + "/" + get_time()
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
    cmd = f"./output >> {this_eval_dir}/output 2>&1"
    if zombie_runtime is not None:
        cmd = f"timeout {zombie_runtime/1e9*20} {cmd}"
    output = subprocess.run(cmd, shell=True, check=False)
    write_to(f"{this_eval_dir}/returncode", str(output.returncode))
    subprocess.run(f"mv output {this_eval_dir}/program", shell=True, check=False)

    r = read(f"{this_eval_dir}")
    if not r.ok:
        subprocess.run(f"cat {this_eval_dir}/output", shell=True, check=False)
        raise
    elif tv_backend == "zombie" and limit['name'] == "no":
        zombie_memory_limit, zombie_runtime = get_spacetime(r)

for x in flatten_nondet(ev).l:
    call(x)
