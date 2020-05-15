all:
	dune build && ./_build/default/src/osat.exe bt ./test/aim.cnf

clean:
	rm -r _build
