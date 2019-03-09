#! /usr/bin/env python3

import os

submit_file_name = 'submit.ml'
if os.path.exists(submit_file_name):
    os.remove(submit_file_name)
submit_file = open(submit_file_name, 'x')


# get generate-object-file from OMakefile
code_files = []
try:
    file = open("./OMakefile")
    flag = False
    for line in file.readlines():
        if flag:
            line = line.strip()
            if line != "":
                code_files.append(line)
            else:
                break
        elif line.strip() == "FILES[] =":
            flag = True
finally:
    file.close()


# write out submit.ml
for cf in code_files:
    fl = open(cf + '.ml', 'r')
    text = (
        "module {} = struct".format(cf[0].upper() + cf[1:]),
        fl.read(),
        "end"
    )
    fl.close()

    text = "\n".join(text) + "\n"
    submit_file.write(text)

main_file = open('main.ml', 'r')
submit_file.write(main_file.read())
main_file.close()

submit_file.close()
