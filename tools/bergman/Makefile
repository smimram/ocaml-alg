all:
	@dune build

test:
	./bergman --variables x,y,z x^2,z^2,xz+zx,xy-yx-y^2,zy-yz-yt,xt-tx-ty,zt-tz-t^2

conflicts:
	menhir --explain parser.mly && less parser.conflicts && rm parser.ml parser.mli parser.conflicts

