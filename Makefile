all:
	dune build \
	&& time ./_build/default/src/osat.exe ./test/aim.cnf 

clean:
	rm -r _build
