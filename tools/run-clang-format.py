
import os
import subprocess

format = r"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\Llvm\x64\bin\clang-format.exe"

for where,dirs,files in os.walk( os.path.join(__file__,"..","..","libs") ):
    todo = [f for f in files if (f.endswith(".h") or f.endswith(".cpp")) and f != "Pch.h" ]
    for t in todo:
        subprocess.run([format, "-verbose", "-style=file", "-i", t], cwd=os.path.abspath(where))
