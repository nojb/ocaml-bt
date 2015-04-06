OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	$(OCAMLBUILD) otorrent/otorrent.byte

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
