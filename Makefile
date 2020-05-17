all:
	dune build
install:
	sudo cp ./_build/default/src/osat.exe /usr/local/bin/osat
	sudo chmod +x /usr/local/bin/osat
clean:
	rm -r _build
