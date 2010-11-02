VERSION ?= dev

.PHONY: clean wc doc web

clean:
	rm -f *.fasl */*.fasl *~ \#*

wc:
	wc -l *.lisp *.asd

release: clean
	mkdir sb-cga-$(VERSION)
	cp NOTES Makefile *.asd *.lisp sb-cga-$(VERSION)
	tar -czvf sb-cga-$(VERSION).tar.gz sb-cga-$(VERSION)
	rm -rf sb-cga-$(VERSION)

doc:
	make -C doc

web: doc
	sbcl --script doc/splice-analytics.lisp < doc/sb-cga.html > tmp.html
	git checkout gh-pages
	mv tmp.html index.html
