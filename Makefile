test-all: test test-subdirs

include Makefile.test

test-subdirs:
	make -C cat test
	make -C litc test
	make -C odump test
