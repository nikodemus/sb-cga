VERSION ?= dev

.PHONY: clean wc

clean:
	rm -f *~ *.fasl

wc:
	wc -l *.lisp *.asd

release: clean
	mkdir sb-cga-$(VERSION)
	cp NOTES Makefile *.asd *.lisp sb-cga-$(VERSION)
	tar -czvf sb-cga-$(VERSION).tar.gz sb-cga-$(VERSION)
	rm -rf sb-cga-$(VERSION)
