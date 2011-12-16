files=$(wildcard *.lisp) $(wildcard sources/*.lisp)

.PHONY: clean all test

all: atonews

test: atonews
	./atonews

clean:
	rm -f ./atonews

atonews: $(files)
	sbcl --eval "(progn (require :asdf) (require :atonews))" \
	     --eval "(load \"make-image.lisp\")"
