ROOTDIR = $(CURDIR)
include $(ROOTDIR)/Makefile.test

LIB = $(shell find lib -name "*.scm" -o -name "*.tst" -o -name "*.sld")
CMD = $(shell find cmd -name "*.scm" -o -name "*.tst" -o -name "*.sld")
SRCS = $(LIB) $(CMD)

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

build:
	chicken-install -n

.PHONY: format test build perms
