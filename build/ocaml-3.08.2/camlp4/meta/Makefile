# $Id: Makefile,v 1.15.2.1 2004/07/12 10:05:21 garrigue Exp $

include ../config/Makefile

INCLUDES=-I ../camlp4 -I ../boot -I $(OTOP)/utils
OCAMLCFLAGS=-warn-error A $(INCLUDES)
OBJS=q_MLast.cmo pa_r.cmo pa_rp.cmo pa_extend.cmo pa_extend_m.cmo pa_macro.cmo pr_dump.cmo
OBJSX=$(OBJS:.cmo=.cmx)
CAMLP4RM=pa_r.cmo pa_rp.cmo pr_dump.cmo
CAMLP4RMX=$(CAMLP4RM:.cmo=.cmx)
SHELL=/bin/sh
COUT=$(OBJS) camlp4r$(EXE)
COPT=$(OBJSX) camlp4r.opt

all: $(COUT)
opt: $(COPT)

camlp4r$(EXE): ../camlp4/camlp4$(EXE) $(CAMLP4RM)
	rm -f camlp4r$(EXE)
	cd ../camlp4; $(MAKE) OTOP=$(OTOP) CAMLP4=../meta/camlp4r$(EXE) CAMLP4M="-I ../meta $(CAMLP4RM)"

camlp4r.opt: $(CAMLP4RMX)
	rm -f camlp4r.opt
	cd ../camlp4; $(MAKE) optp4 OTOP=$(OTOP) CAMLP4OPT=../meta/camlp4r.opt CAMLP4M="-I ../meta $(CAMLP4RMX)"

clean::
	rm -f *.cm* *.pp[io] *.o *.bak .*.bak $(COUT) $(COPT)

depend:
	cp .depend .depend.bak
	> .depend
	@for i in *.mli *.ml; do \
	  ../tools/apply.sh pr_depend.cmo -- $(INCLUDES) $$i | \
	  sed -e 's| \.\./\.\.| $$(OTOP)|g' >> .depend; \
	done

promote:
	cp $(COUT) pa_extend.cmi ../boot/.

compare:
	@for j in $(COUT); do \
		if cmp $$j ../boot/$$j; then :; else exit 1; fi; \
	done

install:
	-$(MKDIR) "$(LIBDIR)/camlp4" "$(BINDIR)"
	cp $(OBJS) "$(LIBDIR)/camlp4/."
	cp pa_macro.cmi pa_extend.cmi "$(LIBDIR)/camlp4/."
	cp camlp4r$(EXE) "$(BINDIR)/."
	if test -f camlp4r.opt; then \
	  cp camlp4r.opt "$(BINDIR)/camlp4r.opt$(EXE)" ;\
	  cp $(OBJSX) "$(LIBDIR)/camlp4/."; \
	  for file in $(OBJSX); do \
	    cp "`echo $$file | sed -e 's/\.cmx$$/.$(O)/'`" "$(LIBDIR)/camlp4/."; \
	  done ; \
	fi

include .depend
