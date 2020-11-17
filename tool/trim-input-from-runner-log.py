#! /usr/bin/env python3

# ローカルrunnerのログから入力を抽出する
# ファイルバスを引数として渡す
# e.g. ./trim-input-from-runner-log.py "${HOME}/tool/tmp/input.txt" "${HOME}/tool/tmp/output.txt"

import sys

args = sys.argv[1:]
inputFilePath = args[0]
outputFilePath = args[1]

cnt = 0
flag = False
with open(inputFilePath) as inputFile:
    with open(outputFilePath, mode='w') as outputFile:
        for line in inputFile.readlines():
            if "=== Read from player" in line:
                flag = False

            if flag:
                outputFile.write(line)

            if "[NEXT_PLAYER_INPUT]" in line:
                if cnt % 2 == 0:
                    flag = True
                cnt += 1
