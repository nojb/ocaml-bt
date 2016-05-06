OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	$(OCAMLBUILD) lib/bt.native

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
