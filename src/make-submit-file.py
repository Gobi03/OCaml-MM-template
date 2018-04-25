#! /usr/bin/env python3

import os

submit_file_name = 'submit.ml'
if os.path.exists(submit_file_name):
    os.remove(submit_file_name)
submit_file = open(submit_file_name, 'x')


# generate file
code_files = ['common', 'entities']

for cf in code_files:
    fl = open(cf + '.ml', 'r')
    text = (
        "module {} = struct".format(cf.capitalize()),
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
