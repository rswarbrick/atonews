files=$(wildcard *.lisp)

all: atonews

test: atonews
	./atonews

atonews: $(files)
	sbcl --eval "(progn (require :asdf) (require :atonews))" \
	     --eval "(load \"make-image.lisp\")"
