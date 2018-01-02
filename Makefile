APP = megametro

ML = $(wildcard *.ml)
MLI = $(wildcard *.mli)

OBJ = $(ML:.ml=.cmo)
INT = $(MLI:.mli=.cmi)

CAMLC = ocamlc

all: $(INT) $(APP)

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmo: %.ml
	$(CAMLC) -c $<

main.cmo: main.ml 
	$(CAMLC) -c $<

$(APP): $(OBJ)
	$(CAMLC) -o $(APP) $^

clean:
	rm -rf $(APP) *.cm[iox] *o *.out 2>/dev/null

-include .depend
