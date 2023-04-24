DESTDIR=../../www/manuals/phylopomp

default:
	$(MAKE) -C vignettes default clean

publish: vignettes clean
	rsync -avz vignettes $(DESTDIR)

clean:
	$(MAKE) -C vignettes clean
