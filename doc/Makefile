.PHONY: clean html include doc

doc: html

clean:
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr

include:
	sbcl \
	--eval '(let ((asdf:*central-registry* (cons #p"../" asdf:*central-registry*))) (require :sb-cga))' \
        --load docstrings.lisp \
	--eval '(sb-texinfo:generate-includes "include/" (list :sb-cga) :base-package :sb-cga)' \
	--eval '(quit)'

html: include
	makeinfo --html --no-split --css=style.css sb-cga.texinfo

