all:
	dune build && ./_build/default/src/osat.exe bt ./test/t2.cnf

clean:
	rm -r _build
