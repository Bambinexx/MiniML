import sys
import os

action = sys.argv[1]

if action == "compile":
    os.system("ocamlopt")
    os.system("rm src/main.cmi")
    os.system("rm src/main.cmo")
    os.system("rm src/lexer.cmi")
    os.system("rm src/lexer.cmo")
    os.system("rm src/parser.cmi")
    os.system("rm src/parser.cmo")

if action == "clean":
    os.system("rm build/main.py")