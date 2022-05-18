all:
	@dune build

conflicts:
	menhir --explain parser.mly && less parser.conflicts && rm parser.ml parser.mli parser.conflicts
