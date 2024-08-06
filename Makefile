test-all: test test-subdirs

include Makefile.test

test-subdirs:
	make -C cat test
	make -C defuse test
	make -C pasm test
