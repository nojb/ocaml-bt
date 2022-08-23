.PHONY: all
all:
	dune build

.PHONY: fmt
fmt:
	dune fmt

.PHONY: clean
clean:
	dune clean
