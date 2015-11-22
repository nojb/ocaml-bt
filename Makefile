OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	$(OCAMLBUILD) lib/client.byte

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
