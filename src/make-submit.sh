#!/bin/sh

file=submit.ml

rm -f ${file}
touch ${file}

# generate file
echo "module Entities = struct" >> ${file}
cat entities.ml >> ${file}
echo "end" >> ${file}

cat main.ml >> ${file}
