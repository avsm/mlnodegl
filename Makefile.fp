# $Id: Makefile.fp,v 1.1 2005/06/16 22:35:27 avsm Exp $

OCAMLMAKEFILE = OCamlMakefile

SOURCES = fp.ml

RESULT = fptest

all: top

run: top
	./${RESULT}.top

include $(OCAMLMAKEFILE)
