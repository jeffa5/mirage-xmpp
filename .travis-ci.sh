#!/usr/bin/env bash

ls -la

chmod +x .travis-opam.sh

./.travis-opam.sh

dune build @src/runtest