#! /usr/bin/env python3

# コンストラクタを引数として渡す
# e.g. "Red | Blue | Green"

import sys

args = sys.argv[1:]
consts = args[0].split(" | ")

print("[1] そのまま")
print("[2] 全大文字")
print("please input the number: ")
com = int(input())
if com < 1 or 2 < com:
    raise NameError("invalid command: " + str(com))

print()
print("function")
for const in consts:
    right = const
    if com == 2:
        right = const.upper()
    res = "  | " + const + " -> \"" + right + "\""
    print(res)
