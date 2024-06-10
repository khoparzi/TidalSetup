mkfile_path := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

prefix=/usr/local

install:
	ln -fs $(mkfile_path)/bin/superdirtvim $(prefix)/bin/superdirtvim

uninstall:
	rm -f $(prefix)/bin/superdirtvim

.PHONY: install uninstall
