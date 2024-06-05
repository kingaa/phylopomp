REPODIR = repo
INCLUDES =
HEADERS = src/decls.h
SESSION_PKGS = datasets,utils,grDevices,graphics,stats,methods,tidyverse,pomp,$(PKG)

include rules.mk

POMP_HEADERS = $(shell $(REXE) -e 'cat(system.file("include",package="pomp"))')

src/decls.h: $(CSOURCE)
	file=`mktemp tmpXXXXXXX.h` && \
	cproto -f2 -I $(R_HOME)/include -I $(POMP_HEADERS) -e $(CSOURCE) > $$file && \
	mv $$file $@
