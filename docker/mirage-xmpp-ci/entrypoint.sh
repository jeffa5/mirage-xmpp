#!/usr/bin/env bash

set -e

echo -e "travis_fold:start:opam_setup"
opam switch 4.05
eval $(opam env)
opam repository set-url default https://opam.ocaml.org
opam update
opam upgrade -y

eval $(opam env)

opam pin add . --no-action
opam depext --yes --update --install mirage-xmpp
opam depext --yes --update --install mirage
echo -e "travis_fold:end:opam_setup"

echo -e "travis_fold:start:unit"
make unit
echo -e "travis_fold:end:unit"

echo -e "travis_fold:start:integration"
make integration
echo -e "travis_fold:end:integration"

echo -e "travis_fold:start:makedocs"
opam install --yes odoc
make doc
echo -e "travis_fold:end:makedocs"

echo -e "travis_fold:start:coverage"
make coverage
echo -e "travis_fold:end:coverage"
