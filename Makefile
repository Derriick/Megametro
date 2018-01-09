APP1 = phase1
APP2 = phase2
APP3 = phase3

CAMLC = ocamlc
.PRECIOUS: %.cmi %.cmo

# SRC = analyse.ml table.ml
SRC = $(wildcard *.mli)
INT = $(SRC:.mli=.cmi)
OBJ = $(SRC:.mli=.cmo)

all: $(APP1) $(APP2) $(APP3)

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmo: %.ml $(INT)
	$(CAMLC) -c $<

%: $(OBJ) %.cmo
	$(CAMLC) -o $@ $^

clean:
	rm -rf $(APP1) $(APP2) $(APP3) *.cm[io] 2>/dev/null
