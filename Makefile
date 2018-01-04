APP1 = phase1
APP2 = phase2
SRC = analyse.ml table.ml

# ML = $(wildcard *.ml)
# MLI = $(wildcard *.mli)

# OBJ = $(ML:.ml=.cmo)
# INT = $(MLI:.mli=.cmi)
OBJ = $(SRC:.ml=.cmo)
INT = $(SRC:.ml=.cmi)

CAMLC = ocamlc

all: $(APP1) $(APP2)

$(APP1).cmo: $(APP1).ml $(INT)
	$(CAMLC) -c $<

$(APP2).cmo: $(APP2).ml $(INT)
	$(CAMLC) -c $<

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmo: %.ml $(INT)
	$(CAMLC) -c $<

$(APP1): $(OBJ) $(APP1).cmo
	$(CAMLC) -o $@ $^

$(APP2): $(OBJ) $(APP2).cmo
	$(CAMLC) -o $@ $^

clean:
	rm -rf $(APP1) $(APP2) *.cm[iox] *o *.out 2>/dev/null
