OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex
GTAR=tar

RESULT = cpf
LIBS = str.cma
OBJECTS = cpfannotation.cmo cpfparse.cmo cpflex.cmo cpf.cmo

all: cpf cpf.top

cpf: $(OBJECTS)
	$(OCAMLC) -o $(RESULT) $(LIBS) $(OBJECTS)

cpf.top: $(OBJECTS)
	ocamlmktop -o $(RESULT).top $(LIBS) $(OBJECTS)

cpfannotation.cmo: cpfannotation.ml
	$(OCAMLC) -c cpfannotation.ml

cpfparse.cmo: cpfparse.mly cpfannotation.cmo
	$(OCAMLYACC) -v cpfparse.mly
	$(OCAMLC) -c cpfparse.mli
	$(OCAMLC) -c cpfparse.ml

cpflex.cmo: cpflex.mll cpfparse.cmo
	$(OCAMLLEX) cpflex.mll
	$(OCAMLC) -c cpflex.ml

cpf.cmo: cpf.ml cpfparse.cmo cpflex.cmo cpfannotation.cmo
	$(OCAMLC) -c cpf.ml

clean: 
	rm -f cpf cpf.top *cmo *cmi 
	rm cpfparse.mli cpfparse.ml cpflex.ml cpfparse.output
