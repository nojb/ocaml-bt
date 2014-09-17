OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all: bt/bt.cma

%.cma:
	$(OCAMLBUILD) $@

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
