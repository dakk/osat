all:
	dune build 
	./_build/default/src/osat.exe test/cbs100/CBS_k3_n100_m403_b10_0.cnf
	# ./_build/default/src/osat.exe test/uf20/uf20-05.cnf
	# && time ./_build/default/src/osat.exe test/cbs100/CBS_k3_n100_m403_b10_0.cnf
	# && time ./_build/default/src/osat.exe test/lit_elim.cnf
	# && time ./_build/default/src/osat.exe test/uf50/uf50-01.cnf
	# https://www.cs.ubc.ca/~hoos/SATLIB/benchm.html
clean:
	rm -r _build
