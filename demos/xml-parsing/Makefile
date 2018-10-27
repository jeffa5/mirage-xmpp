BUILDDIR = _build
SCRIPTSDIR = scripts
SRCDIR = src

.PHONY: all build run clean
all: run

build: clean
	mkdir -p $(BUILDDIR)
	cp $(SCRIPTSDIR)/* $(BUILDDIR)
	cp $(SRCDIR)/* $(BUILDDIR)
	cd $(BUILDDIR) && ./build

run: build
	cd $(BUILDDIR) && ./run

clean:
	rm -rf $(BUILDDIR)