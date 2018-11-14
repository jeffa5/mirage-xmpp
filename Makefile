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
	cp _build/default/mirage/xmpp mirage

# run the integration tests
.PHONY: integration
integration: mirage
	sudo dune build @integration/runtest

# run the unikernel built by mirage
.PHONY: run
run: mirage
	sudo mirage/xmpp -l "*:debug"

.PHONY: tap
tap:
	sudo ifconfig tap0 10.0.0.1 up

.PHONY: demo
demo: tap
	echo "<stream:stream from='juliet@im.example.com' to='im.example.com' version='1.0' xml:lang='en' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>" | nc -nw1 10.0.0.2 8080

.PHONY: test
test: unit integration

# promote the files, typically for expect tests
.PHONY: promote
promote:
	dune promote

# clean the repository
.PHONY: clean
clean:
	dune clean

.PHONY: pages
pages:
	mkdir -p pages

.PHONY: coverage
coverage: clean pages
	rm -rf pages/coverage
	BISECT_ENABLE=YES dune build @runtest --force
	bisect-ppx-report -I _build/default/src -html pages/coverage `find . -name 'bisect*.out'`

.PHONY: doc
doc: clean pages
	rm -rf pages/docs
	dune build @doc
	cp -r _build/default/_doc/_html pages/docs

.PHONY: format
format: clean
	dune build @fmt --auto-promote

.PHONY: check
check:
	dune build @check