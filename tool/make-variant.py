#! /usr/bin/env python3

# ファイル名を引数として渡し、その中身から直和型を作る
# 命名は先頭大文字
# 各行が各値に当たる

import sys

args = sys.argv[1:]
fileName = args[0]

try:
    file = open(fileName)
    res = []
    for line in file.readlines():
        line = line.strip()
        if line:
            res.append(line.capitalize())
    
    print(" | ".join(list(res)))
finally:
    file.close()
