all:
	dune build && ./_build/default/src/osat.exe

clean:
	rm -r _build
