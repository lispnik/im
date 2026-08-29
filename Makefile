# Build and test the IM Common Lisp bindings.
#
#   make          -- build bin/im (embeds the SBCL core)
#   make test     -- run the FiveAM suite
#   make bindings -- regenerate src/ffi/ from the IM headers
#   make clean    -- remove bin/ and this tree's fasl cache
#
# The binary embeds an SBCL core, so build it on the platform you will run it
# on. It still dlopens libim at startup: set IM_LIBRARY_PATH, or put the
# libraries in lib/ beside bin/, which is the layout the release tarball uses.

SBCL       ?= sbcl
SBCL_FLAGS := --non-interactive --no-userinit --no-sysinit

# Where a source checkout of the C library lives, for `make bindings'.
IM_SOURCE ?= ../../tecgraf/tecgraf-im

# Hermetic source registry: this tree and its vendored ocicl/ dependencies,
# and nothing else the machine happens to have lying around. A missing
# dependency should fail loudly here rather than resolve to a neighbour's copy.
BOOT := --eval "(require :asdf)" \
        --eval "(asdf:initialize-source-registry \`(:source-registry (:tree ,(truename \"./\")) :ignore-inherited-configuration))"

SRC := im.asd $(wildcard src/*.lisp) $(wildcard src/ffi/*.lisp) $(wildcard src/cli/*.lisp)
BIN := bin/im

.PHONY: all test bindings clean help
.DEFAULT_GOAL := all

all: $(BIN)

$(BIN): $(SRC)
	@mkdir -p bin
	$(SBCL) $(SBCL_FLAGS) $(BOOT) --eval '(asdf:make :im/cli)'
	@ls -lh $(BIN)

test:
	$(SBCL) $(SBCL_FLAGS) $(BOOT) --eval '(asdf:test-system :im)'

# Regenerates src/ffi/ in place. The generator overwrites everything it owns,
# so review the diff rather than assuming it is a no-op.
bindings:
	$(SBCL) $(SBCL_FLAGS) $(BOOT) --load tools/gen-bindings.lisp \
	  --eval '(im.gen:generate "$(IM_SOURCE)")'

clean:
	rm -rf bin
	rm -rf $(HOME)/.cache/common-lisp/*$(shell pwd)

help:
	@sed -n '2,12p' Makefile
