#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.8 2000/08/10 09:58:08 xleroy Exp $

# Makefile for the parser generator.

include ../config/Makefile

CC=$(BYTECC)
CFLAGS=-O -DNDEBUG $(BYTECCCOMPOPTS)

OBJS= closure.o error.o lalr.o lr0.o main.o mkpar.o output.o reader.o \
  skeleton.o symtab.o verbose.o warshall.o

all: ocamlyacc$(EXE)

ocamlyacc$(EXE): $(OBJS)
	$(CC) $(CFLAGS) $(CCLINKFLAGS) -o ocamlyacc $(OBJS)

clean:
	rm -f *.o ocamlyacc$(EXE) *~

depend:

closure.o: defs.h
error.o: defs.h
lalr.o: defs.h
lr0.o: defs.h
main.o: defs.h
mkpar.o: defs.h
output.o: defs.h
reader.o: defs.h
skeleton.o: defs.h
symtab.o: defs.h
verbose.o: defs.h
warshall.o: defs.h
