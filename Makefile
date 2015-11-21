OCAMLBUILD = ocamlbuild -classic-display -use-ocamlfind

all:
	$(OCAMLBUILD) lib/client.byte # otorrent/otorrent.byte

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
