CC=gcc
LEX=flex
YACC=bison
CFLAGS=-c -Wall

m1: m1parser.o m1lexer.o m1_ast.o m1_eval.o
	$(CC) -o m1 m1parser.o m1lexer.o m1_ast.o m1_eval.o

m1lexer.o: m1lexer.c
	$(CC) $(CFLAGS) m1lexer.c

m1parser.o: m1parser.c
	$(CC) $(CFLAGS) m1parser.c	

m1parser.c: m1.y
	$(YACC) m1.y

m1lexer.c: m1.l
	$(LEX) m1.l	
	
m1_ast.o: m1_ast.c m1_ast.h
	$(CC) $(CFLAGS) m1_ast.c

m1_eval.o: m1_eval.c m1_eval.h
	$(CC) $(CFLAGS) m1_eval.c	

clean:
	$(RM) -rf m1parser.c \
		m1lexer.c \
		*.o