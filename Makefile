.PHONY: clean wc

clean:
	rm *~ *.fasl

wc:
	wc -l *.lisp *.asd
