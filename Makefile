# ------------------------------------------------------------------------------
# Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
# SPDX-License-Identifier: 0BSD
# ------------------------------------------------------------------------------
ROOTDIR = $(CURDIR)
include $(ROOTDIR)/Makefile.test

PREFIX  = /usr/local
LIB     = $(shell find lib -name "*.scm" -o -name "*.tst" -o -name "*.sld")
CMD     = $(shell find cmd -name "*.scm" -o -name "*.tst" -o -name "*.sld")
SRCS    = $(LIB) $(CMD)

default: format perms test build

test:
	@$(MAKE) -C lib test
	@$(MAKE) -C cmd test

format: $(SRCS)
	for f in $(SRCS); do \
		cat $$f | schematic-format -b > tmpfile; \
		mv tmpfile $$f; \
	done

perms:
	for f in $(CMD); do chmod 755 $$f; done

VERSION:
	@cmd/build-version | tee $@

build: VERSION
	VERSION="$(shell cat VERSION | cut -d- -f1)" && \
		sed -i .bak "s/(version.*)/(version \"$$VERSION\")/g" \
		kittens.egg
	VERSION="$(shell cat VERSION)" && \
		sed -i .bak "s/^VERSION=.*$$/VERSION=$$VERSION/g" \
		cmd/kittens

	chicken-install -n

clean:
	rm -rf kitten.* kittens.*.* *.link \
		$(notdir $(basename $(CMD))) VERSION

install: build
	@mkdir -p $(PREFIX)/bin
	@mkdir -p $(PREFIX)/man/man1
	@install ./explode $(PREFIX)/bin/
	@install cmd/kittens $(PREFIX)/bin/
	@install man/kittens.1 $(PREFIX)/man/man1/

.PHONY: format test build perms clean install
