REXE = $(shell which R) -s
RSCRIPT = $(shell which Rscript) --vanilla
RM = rm -f
CP = cp

default: index.html lbdp_compar.html moran.html seir.html

%.tex: %.Rnw
	$(RSCRIPT) -e "library(knitr); knit(\"$*.Rnw\")"

%.R: %.Rnw
	$(RSCRIPT) -e "library(knitr); purl(\"$*.Rnw\")"

%.pdf: %.tex
	$(PDFLATEX) $*; $(BIBTEX) $*; $(PDFLATEX) $*; $(PDFLATEX) $*

%.bbl: %.tex
	$(PDFLATEX) $*; $(BIBTEX) $*

%.idx: %.tex
	$(PDFLATEX) $*

%.ind: %.idx
	$(MAKEIDX) $*

%.html: %.Rmd
	$(RSCRIPT) -e "rmarkdown::render(\"$*.Rmd\",output_format=\"html_document\")"

%.R: %.Rmd
	$(RSCRIPT) -e "knitr::purl(\"$*.Rmd\",output=\"$*.R\",documentation=2)"

clean:
	$(RM) -r tmp
