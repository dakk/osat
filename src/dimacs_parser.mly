%token <int> VAR
%token <int> NVAR

%token C P EOL

%start <Bexp.p> program

%%

program:


line:
	| C ANYSTRING EOL