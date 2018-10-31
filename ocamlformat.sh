#!/usr/bin/env bash

FILES=$(find . -name "*.ml" -type f)
for f in $FILES; do
    ocamlformat --ocp-indent-compat --inplace --profile=janestreet $f
done

FILES=$(find . -name "*.mli" -type f)
for f in $FILES; do
    ocamlformat --ocp-indent-compat --inplace --profile=janestreet $f
done