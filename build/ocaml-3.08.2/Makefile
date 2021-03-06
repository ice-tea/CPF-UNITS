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

# $Id: Makefile,v 1.186.2.4 2004/07/16 16:11:33 doligez Exp $

# The main Makefile

include config/Makefile
include stdlib/StdlibModules

CAMLC=boot/ocamlrun boot/ocamlc -nostdlib -I boot
CAMLOPT=boot/ocamlrun ./ocamlopt -nostdlib -I stdlib
COMPFLAGS=-warn-error A $(INCLUDES)
LINKFLAGS=

CAMLYACC=boot/ocamlyacc
YACCFLAGS=-v
CAMLLEX=boot/ocamlrun boot/ocamllex
CAMLDEP=boot/ocamlrun tools/ocamldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=byterun/ocamlrun
SHELL=/bin/sh
MKDIR=mkdir -p

INCLUDES=-I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver \
         -I toplevel

UTILS=utils/misc.cmo utils/tbl.cmo utils/config.cmo \
  utils/clflags.cmo utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
  utils/consistbl.cmo

OPTUTILS=$(UTILS)

PARSING=parsing/linenum.cmo parsing/location.cmo parsing/longident.cmo \
  parsing/syntaxerr.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo

TYPING=typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo typing/env.cmo \
  typing/typedtree.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/includecore.cmo \
  typing/includemod.cmo typing/parmatch.cmo \
  typing/typetexp.cmo typing/stypes.cmo typing/typecore.cmo \
  typing/typedecl.cmo typing/typeclass.cmo \
  typing/typemod.cmo

COMP=bytecomp/lambda.cmo bytecomp/printlambda.cmo \
  bytecomp/typeopt.cmo bytecomp/switch.cmo bytecomp/matching.cmo \
  bytecomp/translobj.cmo bytecomp/translcore.cmo \
  bytecomp/translclass.cmo bytecomp/translmod.cmo \
  bytecomp/simplif.cmo bytecomp/runtimedef.cmo

BYTECOMP=bytecomp/meta.cmo bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/opcodes.cmo bytecomp/emitcode.cmo \
  bytecomp/bytesections.cmo bytecomp/dll.cmo bytecomp/symtable.cmo \
  bytecomp/bytelink.cmo bytecomp/bytelibrarian.cmo bytecomp/bytepackager.cmo

ASMCOMP=asmcomp/arch.cmo asmcomp/cmm.cmo asmcomp/printcmm.cmo \
  asmcomp/reg.cmo asmcomp/mach.cmo asmcomp/proc.cmo \
  asmcomp/clambda.cmo asmcomp/compilenv.cmo \
  asmcomp/closure.cmo asmcomp/cmmgen.cmo \
  asmcomp/printmach.cmo asmcomp/selectgen.cmo asmcomp/selection.cmo \
  asmcomp/comballoc.cmo asmcomp/liveness.cmo \
  asmcomp/spill.cmo asmcomp/split.cmo \
  asmcomp/interf.cmo asmcomp/coloring.cmo \
  asmcomp/reloadgen.cmo asmcomp/reload.cmo \
  asmcomp/printlinear.cmo asmcomp/linearize.cmo \
  asmcomp/schedgen.cmo asmcomp/scheduling.cmo \
  asmcomp/emitaux.cmo asmcomp/emit.cmo asmcomp/asmgen.cmo \
  asmcomp/asmlink.cmo asmcomp/asmlibrarian.cmo asmcomp/asmpackager.cmo

DRIVER=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  driver/main_args.cmo driver/main.cmo

OPTDRIVER= driver/pparse.cmo driver/opterrors.cmo driver/optcompile.cmo \
  driver/optmain.cmo

TOPLEVEL=driver/pparse.cmo driver/errors.cmo driver/compile.cmo \
  toplevel/genprintval.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo toplevel/topmain.cmo

TOPLEVELLIB=toplevel/toplevellib.cma
TOPLEVELSTART=toplevel/topstart.cmo

COMPOBJS=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(DRIVER)

TOPLIB=$(UTILS) $(PARSING) $(TYPING) $(COMP) $(BYTECOMP) $(TOPLEVEL)

TOPOBJS=$(TOPLEVELLIB) $(TOPLEVELSTART)

OPTOBJS=$(OPTUTILS) $(PARSING) $(TYPING) $(COMP) $(ASMCOMP) $(OPTDRIVER)

EXPUNGEOBJS=utils/misc.cmo utils/tbl.cmo \
  utils/config.cmo utils/clflags.cmo \
  typing/ident.cmo typing/path.cmo typing/types.cmo typing/btype.cmo \
  typing/predef.cmo bytecomp/runtimedef.cmo bytecomp/bytesections.cmo \
  bytecomp/dll.cmo bytecomp/meta.cmo bytecomp/symtable.cmo toplevel/expunge.cmo

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

# For users who don't read the INSTALL file
defaultentry:
	@echo "Please refer to the installation instructions in file INSTALL."
	@echo "If you've just unpacked the distribution, something like"
	@echo "	./configure"
	@echo "	make world"
	@echo "	make opt"
	@echo "	make install"
	@echo "should work.  But see the file INSTALL for more details."

# Recompile the system using the bootstrap compiler
all: runtime ocamlc ocamllex ocamlyacc ocamltools library ocaml \
  otherlibraries camlp4out $(DEBUGGER) ocamldoc

# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.

# Compile everything the first time
world: coldstart all

# Compile also native code compiler and libraries, fast
world.opt: coldstart opt.opt

# Core bootstrapping cycle
coreboot:
# Save the original bootstrap compiler
	$(MAKE) backup
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/ocamlrun and produces bytecode for
# byterun/ocamlrun
	$(MAKE) promote-cross
# Rebuild ocamlc and ocamllex (run on byterun/ocamlrun)
	$(MAKE) partialclean
	$(MAKE) ocamlc ocamllex
# Rebuild the library (using byterun/ocamlrun ./ocamlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) promote
# Rebuild the core system
	$(MAKE) partialclean
	$(MAKE) core
# Check if fixpoint reached
	$(MAKE) compare

# Bootstrap and rebuild the whole system.
bootstrap:
	$(MAKE) coreboot
	$(MAKE) all
	$(MAKE) compare

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

# Start up the system from the distribution compiler
coldstart:
	cd byterun; $(MAKE) all
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	cd yacc; $(MAKE) all
	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
	cd stdlib; $(MAKE) COMPILER=../boot/ocamlc all
	cd stdlib; cp $(LIBFILES) ../boot
	if test -f boot/libcamlrun.a; then :; else \
          ln -s ../byterun/libcamlrun.a boot/libcamlrun.a; fi
	if test -d stdlib/caml; then :; else \
          ln -s ../byterun stdlib/caml; fi

# Build the core system: the minimum needed to make depend and bootstrap
core : runtime ocamlc ocamllex ocamlyacc ocamltools library

# Save the current bootstrap compiler
MAXSAVED=boot/Saved/Saved.prev/Saved.prev/Saved.prev/Saved.prev/Saved.prev
backup:
	if test -d boot/Saved; then : ; else mkdir boot/Saved; fi
	if test -d $(MAXSAVED); then rm -r $(MAXSAVED); else : ; fi
	mv boot/Saved boot/Saved.prev
	mkdir boot/Saved
	mv boot/Saved.prev boot/Saved/Saved.prev
	cp boot/ocamlrun$(EXE) boot/Saved
	mv boot/ocamlc boot/ocamllex boot/ocamlyacc$(EXE) boot/Saved
	cd boot; cp $(LIBFILES) Saved

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
promote-cross:
	cp ocamlc boot/ocamlc
	cp lex/ocamllex boot/ocamllex
	cp yacc/ocamlyacc$(EXE) boot/ocamlyacc$(EXE)
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
promote: promote-cross
	cp byterun/ocamlrun$(EXE) boot/ocamlrun$(EXE)

# Restore the saved bootstrap compiler if a problem arises
restore:
	mv boot/Saved/* boot
	rmdir boot/Saved
	mv boot/Saved.prev boot/Saved

# Check if fixpoint reached
compare:
	@if cmp boot/ocamlc ocamlc && cmp boot/ocamllex lex/ocamllex; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
        else echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	fi

# Remove old bootstrap compilers
cleanboot:
	rm -rf boot/Saved/Saved.prev/*

# Compile the native-code compiler
opt-core:runtimeopt ocamlopt libraryopt
opt: runtimeopt ocamlopt libraryopt otherlibrariesopt camlp4opt

# Native-code versions of the tools
opt.opt: checkstack core ocaml opt-core ocamlc.opt otherlibraries camlp4out \
	 $(DEBUGGER) ocamldoc ocamlopt.opt otherlibrariesopt \
	 camlp4opt ocamllex.opt ocamltoolsopt.opt camlp4optopt ocamldoc.opt

# Installation
install: FORCE
	if test -d $(BINDIR); then : ; else $(MKDIR) $(BINDIR); fi
	if test -d $(LIBDIR); then : ; else $(MKDIR) $(LIBDIR); fi
	if test -d $(STUBLIBDIR); then : ; else $(MKDIR) $(STUBLIBDIR); fi
	if test -d $(MANDIR)/man$(MANEXT); then : ; else $(MKDIR) $(MANDIR)/man$(MANEXT); fi
	cd $(LIBDIR); rm -f dllbigarray.so dlllabltk.so dllnums.so \
          dllthreads.so dllunix.so dllgraphics.so dllmldbm.so dllstr.so \
          dlltkanim.so
	cd byterun; $(MAKE) install
	cp ocamlc $(BINDIR)/ocamlc$(EXE)
	cp ocaml $(BINDIR)/ocaml$(EXE)
	cd stdlib; $(MAKE) install
	cp lex/ocamllex $(BINDIR)/ocamllex$(EXE)
	cp yacc/ocamlyacc$(EXE) $(BINDIR)/ocamlyacc$(EXE)
	cp toplevel/toplevellib.cma $(LIBDIR)/toplevellib.cma
	cp expunge $(LIBDIR)/expunge$(EXE)
	cp typing/outcometree.cmi typing/outcometree.mli $(LIBDIR)
	cp toplevel/topstart.cmo $(LIBDIR)
	cp toplevel/toploop.cmi toplevel/topdirs.cmi toplevel/topmain.cmi $(LIBDIR)
	cd tools; $(MAKE) install
	-cd man; $(MAKE) install
	for i in $(OTHERLIBRARIES); do \
          (cd otherlibs/$$i; $(MAKE) install) || exit $$?; \
        done
	cd ocamldoc; $(MAKE) install
	if test -f ocamlopt; then $(MAKE) installopt; else :; fi
	cd camlp4; $(MAKE) install BINDIR=$(BINDIR) LIBDIR=$(LIBDIR) MANDIR=$(MANDIR)
	if test -f debugger/ocamldebug; then (cd debugger; $(MAKE) install); \
	   else :; fi

# Installation of the native-code compiler
installopt:
	cd asmrun; $(MAKE) install
	cp ocamlopt $(BINDIR)/ocamlopt$(EXE)
	cd stdlib; $(MAKE) installopt
	cd ocamldoc; $(MAKE) installopt
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) installopt) || exit $$?; done
	if test -f ocamlc.opt; \
	  then cp ocamlc.opt $(BINDIR)/ocamlc.opt$(EXE); else :; fi
	if test -f ocamlopt.opt; \
	  then cp ocamlopt.opt $(BINDIR)/ocamlopt.opt$(EXE); else :; fi
	if test -f lex/ocamllex.opt; \
	  then cp lex/ocamllex.opt $(BINDIR)/ocamllex.opt$(EXE); else :; fi

clean:: partialclean

# The compiler

ocamlc: $(COMPOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamlc $(COMPOBJS)
	@sed -e 's|@compiler@|$$topdir/boot/ocamlrun $$topdir/ocamlc|' \
	  driver/ocamlcomp.sh.in > ocamlcomp.sh
	@chmod +x ocamlcomp.sh

partialclean::
	rm -f ocamlc ocamlcomp.sh

# The native-code compiler

ocamlopt: $(OPTOBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamlopt $(OPTOBJS)
	@sed -e 's|@compiler@|$$topdir/boot/ocamlrun $$topdir/ocamlopt|' \
	  driver/ocamlcomp.sh.in > ocamlcompopt.sh
	@chmod +x ocamlcompopt.sh

partialclean::
	rm -f ocamlopt ocamlcompopt.sh

# The toplevel

ocaml: $(TOPOBJS) expunge
	$(CAMLC) $(LINKFLAGS) -linkall -o ocaml.tmp $(TOPOBJS)
	- $(CAMLRUN) ./expunge ocaml.tmp ocaml $(PERVASIVES)
	rm -f ocaml.tmp

toplevel/toplevellib.cma: $(TOPLIB)
	$(CAMLC) -a -o $@ $(TOPLIB)

partialclean::
	rm -f ocaml toplevel/toplevellib.cma

# The configuration file

utils/config.ml: utils/config.mlp config/Makefile
	@rm -f utils/config.ml
	sed -e 's|%%LIBDIR%%|$(LIBDIR)|' \
            -e 's|%%BYTERUN%%|$(BINDIR)/ocamlrun|' \
            -e 's|%%CCOMPTYPE%%|cc|' \
            -e 's|%%BYTECC%%|$(BYTECC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)|' \
            -e 's|%%BYTELINK%%|$(BYTECC) $(BYTECCLINKOPTS)|' \
            -e 's|%%NATIVECC%%|$(NATIVECC) $(NATIVECCCOMPOPTS)|' \
            -e 's|%%NATIVELINK%%|$(NATIVECC) $(NATIVECCLINKOPTS)|' \
            -e 's|%%PARTIALLD%%|ld -r $(NATIVECCLINKOPTS)|' \
            -e 's|%%PACKLD%%|ld -r $(NATIVECCLINKOPTS)|' \
            -e 's|%%BYTECCLIBS%%|$(BYTECCLIBS)|' \
            -e 's|%%NATIVECCLIBS%%|$(NATIVECCLIBS)|' \
            -e 's|%%RANLIBCMD%%|$(RANLIBCMD)|' \
            -e 's|%%BINUTILS_NM%%|$(BINUTILS_NM)|' \
            -e 's|%%CC_PROFILE%%|$(CC_PROFILE)|' \
            -e 's|%%BINUTILS_OBJCOPY%%|$(BINUTILS_OBJCOPY)|' \
            -e 's|%%ARCH%%|$(ARCH)|' \
            -e 's|%%MODEL%%|$(MODEL)|' \
            -e 's|%%SYSTEM%%|$(SYSTEM)|' \
            -e 's|%%EXT_OBJ%%|.o|' \
            -e 's|%%EXT_ASM%%|.s|' \
            -e 's|%%EXT_LIB%%|.a|' \
            -e 's|%%EXT_DLL%%|.so|' \
            utils/config.mlp > utils/config.ml
	@chmod -w utils/config.ml

partialclean::
	rm -f utils/config.ml

beforedepend:: utils/config.ml

# The parser

parsing/parser.mli parsing/parser.ml: parsing/parser.mly
	$(CAMLYACC) $(YACCFLAGS) parsing/parser.mly

partialclean::
	rm -f parsing/parser.mli parsing/parser.ml parsing/parser.output

beforedepend:: parsing/parser.mli parsing/parser.ml

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) parsing/lexer.mll

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The auxiliary lexer for counting line numbers

parsing/linenum.ml: parsing/linenum.mll
	$(CAMLLEX) parsing/linenum.mll

partialclean::
	rm -f parsing/linenum.ml

beforedepend:: parsing/linenum.ml

# The bytecode compiler compiled with the native-code compiler

ocamlc.opt: $(COMPOBJS:.cmo=.cmx)
	cd asmrun; $(MAKE) meta.o dynlink.o
	$(CAMLOPT) $(LINKFLAGS) -ccopt "$(BYTECCLINKOPTS)" -o ocamlc.opt \
          $(COMPOBJS:.cmo=.cmx) \
          asmrun/meta.o asmrun/dynlink.o -cclib "$(BYTECCLIBS)"
	@sed -e 's|@compiler@|$$topdir/ocamlc.opt|' \
	  driver/ocamlcomp.sh.in > ocamlcomp.sh
	@chmod +x ocamlcomp.sh

partialclean::
	rm -f ocamlc.opt

# The native-code compiler compiled with itself

ocamlopt.opt: $(OPTOBJS:.cmo=.cmx)
	$(CAMLOPT) $(LINKFLAGS) -o ocamlopt.opt $(OPTOBJS:.cmo=.cmx)
	@sed -e 's|@compiler@|$$topdir/ocamlopt.opt|' \
	  driver/ocamlcomp.sh.in > ocamlcompopt.sh
	@chmod +x ocamlcompopt.sh

partialclean::
	rm -f ocamlopt.opt

$(OPTOBJS:.cmo=.cmx): ocamlopt

# The numeric opcodes

bytecomp/opcodes.ml: byterun/instruct.h
	sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' byterun/instruct.h | \
        awk -f tools/make-opcodes > bytecomp/opcodes.ml

partialclean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# The predefined exceptions and primitives

byterun/primitives:
	cd byterun; $(MAKE) primitives

bytecomp/runtimedef.ml: byterun/primitives byterun/fail.h
	(echo 'let builtin_exceptions = [|'; \
	 sed -n -e 's|.*/\* \("[A-Za-z_]*"\) \*/$$|  \1;|p' byterun/fail.h | \
	 sed -e '$$s/;$$//'; \
         echo '|]'; \
         echo 'let builtin_primitives = [|'; \
         sed -e 's/.*/  "&";/' -e '$$s/;$$//' byterun/primitives; \
	 echo '|]') > bytecomp/runtimedef.ml

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	ln -s $(ARCH)/arch.ml asmcomp/arch.ml

partialclean::
	rm -f asmcomp/arch.ml

beforedepend:: asmcomp/arch.ml

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	ln -s $(ARCH)/proc.ml asmcomp/proc.ml

partialclean::
	rm -f asmcomp/proc.ml

beforedepend:: asmcomp/proc.ml

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	ln -s $(ARCH)/selection.ml asmcomp/selection.ml

partialclean::
	rm -f asmcomp/selection.ml

beforedepend:: asmcomp/selection.ml

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	ln -s $(ARCH)/reload.ml asmcomp/reload.ml

partialclean::
	rm -f asmcomp/reload.ml

beforedepend:: asmcomp/reload.ml

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	ln -s $(ARCH)/scheduling.ml asmcomp/scheduling.ml

partialclean::
	rm -f asmcomp/scheduling.ml

beforedepend:: asmcomp/scheduling.ml

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp tools/cvt_emit
	$(CAMLRUN) tools/cvt_emit < asmcomp/$(ARCH)/emit.mlp > asmcomp/emit.ml \
        || { rm -f asmcomp/emit.ml; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

tools/cvt_emit: tools/cvt_emit.mll
	cd tools; $(MAKE) CAMLC="../$(CAMLRUN) ../ocamlc -I ../stdlib" cvt_emit

# The "expunge" utility

expunge: $(EXPUNGEOBJS)
	$(CAMLC) $(LINKFLAGS) -o expunge $(EXPUNGEOBJS)

partialclean::
	rm -f expunge

# The runtime system for the bytecode compiler

runtime:
	cd byterun; $(MAKE) all
	if test -f stdlib/libcamlrun.a; then :; else \
          ln -s ../byterun/libcamlrun.a stdlib/libcamlrun.a; fi
clean::
	cd byterun; $(MAKE) clean
	rm -f stdlib/libcamlrun.a
	rm -f stdlib/caml
alldepend::
	cd byterun; $(MAKE) depend

# The runtime system for the native-code compiler

runtimeopt:
	cd asmrun; $(MAKE) all
	if test -f stdlib/libasmrun.a; then :; else \
          ln -s ../asmrun/libasmrun.a stdlib/libasmrun.a; fi
clean::
	cd asmrun; $(MAKE) clean
	rm -f stdlib/libasmrun.a
alldepend::
	cd asmrun; $(MAKE) depend

# The library

library: ocamlc
	cd stdlib; $(MAKE) all
library-cross:
	cd stdlib; $(MAKE) RUNTIME=../byterun/ocamlrun all
libraryopt:
	cd stdlib; $(MAKE) allopt
partialclean::
	cd stdlib; $(MAKE) clean
alldepend::
	cd stdlib; $(MAKE) depend

# The lexer and parser generators

ocamllex: ocamlyacc ocamlc
	cd lex; $(MAKE) all
ocamllex.opt: ocamlopt
	cd lex; $(MAKE) allopt
partialclean::
	cd lex; $(MAKE) clean
alldepend::
	cd lex; $(MAKE) depend

ocamlyacc:
	cd yacc; $(MAKE) all
clean::
	cd yacc; $(MAKE) clean

# Tools

ocamltools: ocamlc ocamlyacc ocamllex
	cd tools; $(MAKE) all
ocamltoolsopt.opt: ocamlc.opt ocamlyacc ocamllex
	cd tools; $(MAKE) opt.opt
partialclean::
	cd tools; $(MAKE) clean
alldepend::
	cd tools; $(MAKE) depend

# OCamldoc

ocamldoc: ocamlc ocamlyacc ocamllex
	cd ocamldoc && $(MAKE) all
ocamldoc.opt: ocamlc.opt ocamlyacc ocamllex
	cd ocamldoc && $(MAKE) opt.opt
partialclean::
	cd ocamldoc && $(MAKE) clean
alldepend::
	cd ocamldoc && $(MAKE) depend

# The extra libraries

otherlibraries:
	for i in $(OTHERLIBRARIES); do \
          (cd otherlibs/$$i; $(MAKE) RUNTIME=$(RUNTIME) all) || exit $$?; \
        done
otherlibrariesopt:
	for i in $(OTHERLIBRARIES); do \
          (cd otherlibs/$$i; $(MAKE) allopt) || exit $$?; \
        done
partialclean::
	for i in $(OTHERLIBRARIES); do \
          (cd otherlibs/$$i; $(MAKE) partialclean); \
        done
clean::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) clean); done
alldepend::
	for i in $(OTHERLIBRARIES); do (cd otherlibs/$$i; $(MAKE) depend); done

# The replay debugger

ocamldebugger: ocamlc ocamlyacc ocamllex
	cd debugger; $(MAKE) all
partialclean::
	cd debugger; $(MAKE) clean
alldepend::
	cd debugger; $(MAKE) depend

# Camlp4

camlp4out: ocamlc
	cd camlp4; $(MAKE) all
camlp4opt: ocamlopt
	cd camlp4; $(MAKE) opt
camlp4optopt: ocamlopt
	cd camlp4; $(MAKE) opt.opt
partialclean::
	cd camlp4; $(MAKE) clean
alldepend::
	cd camlp4; $(MAKE) depend

# Check that the stack limit is reasonable.

checkstack:
	@if $(BYTECC) -o tools/checkstack tools/checkstack.c; \
	  then tools/checkstack; \
	  else :; \
	fi
	@rm -f tools/checkstack

# Make MacOS X package

.PHONY: package-macosx

package-macosx:
	make BINDIR="`pwd`"/package-macosx/root$(BINDIR) \
	     LIBDIR="`pwd`"/package-macosx/root$(LIBDIR) \
	     MANDIR="`pwd`"/package-macosx/root$(MANDIR) install
	tools/make-package-macosx
clean::
	rm -rf package-macosx/root package-macosx/*.pkg package-macosx/*.dmg

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<

partialclean::
	rm -f utils/*.cm[iox] utils/*.[so] utils/*~
	rm -f parsing/*.cm[iox] parsing/*.[so] parsing/*~
	rm -f typing/*.cm[iox] typing/*.[so] typing/*~
	rm -f bytecomp/*.cm[iox] bytecomp/*.[so] bytecomp/*~
	rm -f asmcomp/*.cm[iox] asmcomp/*.[so] asmcomp/*~
	rm -f driver/*.cm[iox] driver/*.[so] driver/*~
	rm -f toplevel/*.cm[iox] toplevel/*.[so] toplevel/*~
	rm -f tools/*.cm[iox] tools/*.[so] tools/*~
	rm -f *~

depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp driver toplevel; \
	 do $(CAMLDEP) $(DEPFLAGS) $$d/*.mli $$d/*.ml; \
	 done) > .depend

alldepend:: depend

FORCE:

include .depend
