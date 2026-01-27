default: process indent

process:
	R < add_model.R

indent:
	@for i in $(wildcard src/*.cc); \
	do \
	    emacs --batch $$i -f mark-whole-buffer -f indent-region -f mark-whole-buffer -f untabify -f save-buffer; \
	done

install:
	cp src/*.cc ../src
	cp R/*.R ../R

.PHONY: default process indent install
