BUILDDIR = _build
SCRIPTSDIR = scripts
SRCDIR = src

.PHONY: all build run test promote clean
all: build run

build: clean
	mkdir -p $(BUILDDIR)
	cp $(SCRIPTSDIR)/* $(BUILDDIR)
	cp $(SRCDIR)/* $(BUILDDIR)
	cd $(BUILDDIR) && ./build

run:
	cd $(BUILDDIR) && ./run

test:
	# Ensure you are running the unikernel!
	sudo ifconfig tap0 10.0.0.1 up
	dune runtest

promote:
	dune promote

clean:
	rm -rf $(BUILDDIR)