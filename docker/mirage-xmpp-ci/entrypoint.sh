#!/usr/bin/env bash

set -e

opam depext --yes --update --install $(./project-deps.sh)

eval $(opam env)

echo -en "travis_fold:start:unit\r"
make unit
echo -en "travis_fold:end:unit\r"

echo -en "travis_fold:start:integration\r"
make integration
echo -en "travis_fold:end:integration\r"

echo -en "travis_fold:start:makedocs\r"
opam install --yes odoc
make doc
echo -en "travis_fold:end:makedocs\r"

echo -en "travis_fold:start:coverage\r"
make coverage
echo -en "travis_fold:end:coverage\r"
