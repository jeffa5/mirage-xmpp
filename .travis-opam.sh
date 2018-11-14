echo -en "travis_fold:start:prepare.ci\r"
# If a fork of these scripts is specified, use that GitHub user instead
fork_user=${FORK_USER:-ocaml}

# If a branch of these scripts is specified, use that branch instead of 'master'
fork_branch=${FORK_BRANCH:-master}

### Bootstrap

set -uex

get() {
	wget https://raw.githubusercontent.com/${fork_user}/ocaml-ci-scripts/${fork_branch}/$@
}

get .travis-ocaml.sh
sh .travis-ocaml.sh

export OPAMYES=1
eval $(opam config env)

opam depext -y conf-m4
opam pin add travis-opam https://github.com/${fork_user}/ocaml-ci-scripts.git#${fork_branch}
cp ~/.opam/$(opam switch show)/bin/ci-opam ~/

opam remove -a travis-opam

mv ~/ci-opam ~/.opam/$(opam switch show)/bin/ci-opam

echo -en "travis_fold:end:prepare.ci\r"
opam config exec -- ci-opam

echo -en "travis_fold:start:runtest\r"
# run unit tests
make unit
echo -en "travis_fold:end:runtest\r"

echo -en "travis_fold:start:integration\r"
opam install mirage
# run integration tests
TRAVIS_BUILD=yes sudo bash -c "make integration"
dune clean
echo -en "travis_fold:end:integration\r"

echo -en "travis_fold:start:makedocs\r"
# make docs
opam install odoc
make doc
echo -en "travis_fold:end:makedocs\r"

echo -en "travis_fold:start:coverage\r"
# make coverage
make coverage
echo -en "travis_fold:end:coverage\r"
