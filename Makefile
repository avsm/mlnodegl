# $Id: Makefile,v 1.7 2005/06/17 23:17:03 avsm Exp $

OCAMLMAKEFILE = OCamlMakefile

LIBRARY = debug.ml utils.ml fonts.ml mouse.ml  gfxutils.ml colours.ml screen.ml widgets.ml dialogs.ml
SOURCES = $(LIBRARY) fp.ml node.ml glnodenet.ml glnode.ml splgui.ml

RESULT = splgui
INCDIRS = +lablgl
LIBS= str
THREADS=yes
OCAMLLDFLAGS= -cclib '-framework Foundation' 

OCAMLBLDFLAGS = lablglut.cma lablgl.cma
OCAMLNLDFLAGS = lablglut.cmxa lablgl.cmxa

all: nc

run: all
	./${RESULT}
    
include $(OCAMLMAKEFILE)
