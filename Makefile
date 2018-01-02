APP = megametro
SRC = main.ml analyse.ml table.ml metro.ml

OBJ = $(SRC:.ml=.cmo)

CAMLC = ocamlc
FLAGS = -c

all: $(APP)

%.cmi: %.mli
	$(CAMLC) $(FLAGS) $<

%.cmo: %.ml %.cmi
	$(CAMLC) $(FLAGS) $<

main.cmo: main.ml 
	$(CAMLC) $(FLAGS) $<

$(APP): $(OBJ)
	$(CAMLC) -o $(APP) $^

clean:
	rm -rf $(APP) *.cm[io] *.out 2>/dev/null

-include .depend
