ML = $(wildcard *.ml)
PROG = $(ML:.ml=)

all:
	@dune build

runtest:
	@dune runtest

braids:
	@dune exec ./braids.exe

quaternion:
	@dune exec ./quaternion.exe

qgroup:
	@dune exec ./qgroup.exe

rig:
	@dune exec ./rig.exe

dihedral:
	@dune exec ./dihedral.exe

monoid:
	@dune exec ./squierMonoid.exe

stl:
	@dune exec ./stl.exe

stl2:
	@dune exec ./stl2.exe

gen:
	@dune exec ./gen.exe

uatao2:
	@dune exec ./uatao2.exe

test:
	@for p in $(PROG); do echo "\n\n##### Running $$p #####\n"; dune exec ./$$p.exe || exit 1; done

pdf:
	@dune exec ./squierGroup.exe
	pdflatex squierGroupFull.tex
