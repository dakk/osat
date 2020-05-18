all:
	dune build
	#./_build/default/src/osat.exe test/uf/uf150-01.cnf
install:
	sudo cp ./_build/default/src/osat.exe /usr/local/bin/osat
	sudo chmod +x /usr/local/bin/osat
clean:
	rm -r _build
