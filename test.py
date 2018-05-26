
import os
import glob
import time
import argparse
import subprocess

benchmarks = (
    ("bench/ack", 4),
    ("bench/ary", ),
    ("bench/fibo", 5),
    ("bench/harmonic",100),
    #("bench/harmonic",10000000),
    ("bench/hello",),
    ("bench/moments",),
    ("bench/nestedloop",5),
    ("bench/nsieve",4)
    )

def time_call(exe, *args):
    cmd = [exe] + list(args)
    print("Running", cmd)
    start = time.time()
    if subprocess.call(cmd) == 0:
        end = time.time()
        print("Time:", end-start)
    else:
        print("FAILED")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--platform", default="x64")
    parser.add_argument("--config", default="Debug")
    parser.add_argument("--test", default=True)
    parser.add_argument("--benchmark", default=False)
    opts = parser.parse_args()
    slipexe = os.path.join(opts.platform, opts.config, "slip.exe")

    if opts.test:
        for test in glob.glob("test/*.slip"):
            time_call(slipexe, test)
    if opts.benchmark:
        for bench in benchmarks:
            time_call(slipexe, *bench)

if __name__=="__main__":
    main()
