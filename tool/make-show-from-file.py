#! /usr/bin/env python3

# ファイルバスを引数として渡す
# e.g. "Red | Blue | Green"

import sys

args = sys.argv[1:]
fileName = args[0]

print("[1] そのまま")
print("[2] 全大文字")
print("please input the number: ")
com = int(input())

if com < 1 or 2 < com:
    raise NameError("invalid command: " + str(com))

try:
    file = open(fileName)
    for line in file.readlines():
        line = line.strip(" |\n")
        right = line
        if com == 2:
            right = const.upper()
        res = "  | " + line + " -> \"" + right + "\""
        print(res)
finally:
    file.close()
