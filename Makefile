ROOTDIR = $(CURDIR)
include $(ROOTDIR)/Makefile.test

test:
	@$(MAKE) -C lib test
