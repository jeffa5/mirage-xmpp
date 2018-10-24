#!/usr/bin/env bash

FILES=$(find . -name "*.ml" -type f)
for f in $FILES; do
    ocamlformat --inplace --profile=janestreet $f
done