import subprocess

subprocess.run("rm -rf log/ output/", shell=True, check=True)
subprocess.run("mkdir log", shell=True, check=True)
subprocess.run("mkdir output", shell=True, check=True)
