.PHONY: all
all: mirage

#
# XMLPARSER
#

# run unit tests for the xmlparser package
.PHONY: unit
unit: clean
	dune build @src/runtest

# build the xmlparser package
.PHONY: build
build: unit
	dune build @install

# install the xmlparser package to opam
.PHONY: install
install: build
	dune install
	opam pin -y .

#
# MIRAGE
#

# build the mirage unikernel
.PHONY: mirage
mirage: install
	dune build @mirage
	cp _build/default/mirage/xmpp mirage

# run the integration tests
.PHONY: integration
integration: mirage
	dune build @integration/runtest

# run the unikernel built by mirage
.PHONY: run
run: mirage
	sudo mirage/xmpp -l "*:debug" > unikernel.log 2>&1
# configure the tap for connecting
.PHONY: tap
tap:
	sudo ip addr add 10.0.0.1/16 dev tap0
	sudo ip link set tap0 up

# promote the files, typically for expect tests
.PHONY: promote
promote:
	dune promote

# clean the repository
.PHONY: clean
clean:
	dune clean

# ensure the pages directory is available
.PHONY: pages
pages:
	mkdir -p pages

# build the coverage report
.PHONY: coverage
coverage: clean pages
	rm -rf pages/coverage
	BISECT_ENABLE=YES dune build @runtest --force
	bisect-ppx-report -I _build/default/src -html pages/coverage `find . -name 'bisect*.out'`

# build the docs
.PHONY: doc
doc: clean pages
	rm -rf pages/docs
	dune build @doc
	cp -r _build/default/_doc/_html pages/docs

# format the files
.PHONY: format
format: clean
	dune build @fmt --auto-promote

# run a dune @check to generate merlin files
.PHONY: check
check:
	dune build @check

.PHONY: docker-build
docker-build:
	docker build -f docker/mirage-xmpp-ci/Dockerfile --build-arg opam_deps="$$(./project-deps.sh)" -t jeffas/mirage-xmpp-ci:latest .

.PHONY: docker-ci
docker-ci: docker-build
	docker run --privileged -v $${PWD}:/home/opam/app jeffas/mirage-xmpp-ci:latest docker/mirage-xmpp-ci/entrypoint.sh
