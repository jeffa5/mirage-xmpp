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

#
# MIRAGE
#

# build the mirage unikernel
.PHONY: mirage
mirage: install
	dune build @mirage

# run the integration tests
.PHONY: integration
integration:
	# Ensure you are running the unikernel!
	sudo ifconfig tap0 10.0.0.1 up
	dune build @test/runtest

# run the unikernel built by mirage
.PHONY: run
run: mirage
	cd _build/default/mirage && sudo ./xmlparse -l "*:debug" &> unikernel-log

# promote the files, typically for expect tests
.PHONY: promote
promote:
	dune promote

# clean the repository
.PHONY: clean
clean:
	dune clean

.PHONY: coverage
coverage:
	rm -rf coverage
	BISECT_ENABLE=YES dune build @src/runtest --force
	bisect-ppx-report -I _build/default/src -html coverage/ `find . -name 'bisect*.out'`

.PHONY: doc
doc:
	rm -rf doc
	dune build @doc
	cp -r _build/default/_doc/_html doc

.PHONY: format
format:
	dune build @fmt
	# --auto-promote