import sys
import os

action = sys.argv[1]

if action == "compile":
    os.system("ocamlc -o build/main src/main.ml")
    os.system("rm src/main.cmi")
    os.system("rm src/main.cmo")

if action == "clean":
    os.system("rm build/main.py")