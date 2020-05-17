# osat

A dummy sat3 solver. This solver uses DPLL algorithm, selecting for branching the 
literal with most occurences.

```
opam install dolmen
make
./_build/default/src/osat.exe path/of/3cnf/file.cnf
```

