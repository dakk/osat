all:
	dune build && time ./_build/default/src/osat.exe bt ./test/uf20/uf20-03.cnf

clean:
	rm -r _build
