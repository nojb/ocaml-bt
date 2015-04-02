OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	$(OCAMLBUILD) otorrent/otorrent.native

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
