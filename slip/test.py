
import glob
import os

for f in glob.glob("bench/*.slip"):
	print(f)
	os.system("Debug\\slip.exe %s" % f)