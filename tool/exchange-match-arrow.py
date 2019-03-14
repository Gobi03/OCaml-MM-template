#! /usr/bin/env python3

# match式の本体部分のアローの左辺と右辺を入れ替える

import sys

args = sys.argv[1:]
fileName = args[0]

try:
    file = open(fileName)
    res = []
    for line in file.readlines():
        if line.strip():
            tokens = line.strip().split(" ")
            tokens[1], tokens[3] = tokens[3], tokens[1]
            res.append(" ".join(tokens))
    print("\n".join(list(res)))
finally:
    file.close()
