
import glob
import os
import time

tests = (
    ("ack", 4),
    ("ary", ),
    ("fibo", 5),
    ("harmonic",100),
    #("harmonic",10000000),
    ("hello",),
    ("moments",),
    ("nestedloop",5),
    ("nsieve",4)
    )


for test in tests:
    cmd = r"x64\\Release\\slip.exe bench\\%s.slip %s" % (test[0],"".join(str(s) for s in test[1:]))
    print("Running", cmd)
    start = time.time()
    if os.system(cmd) == 0:
        end = time.time()
        print("Time:", end-start)
    else:
        print("FAILED")