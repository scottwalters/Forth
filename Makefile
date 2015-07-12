all:		forth forth.mem

forth:		forth.c
		cc forth.c -g -o forth

nf:		nf.o lex.yy.o
		cc -g -o nf nf.o lex.yy.o

nf.o:		nf.c forth.lex.h
		cc -g -c nf.c

lex.yy.o:	lex.yy.c forth.lex.h
		cc -g -c lex.yy.c

lex.yy.c:	forth.lex
		flex -l forth.lex

forth.mem:	forth.dict nf
		mv -f forth.mem forth.mem.last || true
		./nf < forth.dict
