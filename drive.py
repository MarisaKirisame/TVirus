from EVAL import *
import datetime
import os
import time
import subprocess

default = {"program": "taba", "backend": NONDET("baseline", "zombie")}

def get_time():
    return datetime.datetime.now().strftime("%m%d_%H%M%S")

def write_to(path, val):
    with open(path, "a") as f:
        f.write(val)

def call(x):
    this_eval_dir = eval_dir + "/" + get_time()
    os.mkdir(this_eval_dir)
    write_to(this_eval_dir + "/config", str(x))
    cmd = f"""mill codegen {x['program']} {x['backend']} {this_eval_dir}/log"""
    print(f"running: {cmd}")
    output = subprocess.run(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False, text=True)
    write_to(f"{this_eval_dir}/output", output.stdout)
    subprocess.run(f"mv output.cpp {this_eval_dir}/output.cpp", shell=True, check=False)
    output = subprocess.run("./output", stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    write_to(f"{this_eval_dir}/output", output.stdout)
    write_to(f"{this_eval_dir}/returncode", str(output.returncode))

def run(ev):
    for x in flatten_nondet(ev).l:
        call(x)

eval_dir = "log/" + get_time()
os.mkdir(eval_dir)

run(default)
