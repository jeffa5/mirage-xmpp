#!/usr/bin/env bash

set -e

opam switch 4.05
opam update
opam upgrade -y

eval $(opam env)

opam depext --yes --update --install dune
opam depext --yes --update --install $(./project-deps.sh)

make mirage

mirage/xmpp --hostname="localhost" -l "info"
