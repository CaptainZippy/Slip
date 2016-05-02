
import glob
import os

tests = (
    ("ack", 4),
    ("ary", ),
    ("fibo", 5),
    ("harmonic",),
    ("hello",),
    ("moments",),
    ("nestedloop",5),
    ("nsieve",4)
    )


for test in tests:
    cmd = r"x64\\Release\\slip.exe bench\\%s.slip %s" % (test[0],"".join(str(s) for s in test[1:]))
    print(cmd)
    os.system(cmd)