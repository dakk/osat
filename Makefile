all:
	dune build && ./_build/default/src/osat.exe bt ./test/bf.cnf

clean:
	rm -r _build
