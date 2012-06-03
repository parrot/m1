CC    = gcc
LEX   = flex
YACC  = bison
DEBUG = true

O     = .o
EXE   =

ifeq ($(DEBUG), true)
CFLAGS = -O0 -g -W -Wall
else
CFLAGS = -O3 -W -Wall
endif

M1_O_FILES = \
	src/m1parser$(O) \
	src/m1lexer$(O) \
	src/ast$(O) \
	src/symtab$(O) \
	src/semcheck$(O) \
	src/stack$(O) \
	src/decl$(O) \
	src/eval$(O) \
	src/instr$(O) \
	src/gencode$(O) \
	src/main$(O) \

m1$(EXE): $(M1_O_FILES)
	$(CC) -I$(@D) -o m1$(EXE) $(M1_O_FILES)

src/m1lexer$(O): src/m1lexer.c
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1lexer.c

src/m1parser$(O): src/m1parser.c
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1parser.c	

src/m1parser.c: src/m1.y src/m1lexer.c
	$(YACC) src/m1.y
	mv m1parser.c src/ 
	mv m1parser.h src/ 

src/m1lexer.c: src/m1.l
	$(LEX) src/m1.l	
	mv m1lexer.c src/ 
	mv m1lexer.h src/ 
	
src/ast$(O): src/ast.c src/ast.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/ast.c

src/eval$(O): src/eval.c src/eval.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/eval.c	

src/symtab$(O): src/symtab.c src/symtab.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/symtab.c

src/instr$(O): src/instr.c src/instr.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/instr.c

src/gencode$(O): src/gencode.c src/gencode.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/gencode.c

src/semcheck$(O): src/semcheck.c src/semcheck.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/semcheck.c

src/stack$(O): src/stack.c src/stack.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/stack.c

src/main$(O): src/m1parser.h src/main.c
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/main.c

src/decl$(O): src/decl.c src/decl.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/decl.c

test: m1$(EXE) t/*.m1
	for file in `ls t/*.m1`; \
	do \
	    echo $$file; \
	    ./run_m1.sh $$file; \
	done

clean:
	$(RM) -rf src/m1parser.* \
		src/m1lexer.* \
		src/*$(O) \
		./m1$(EXE) \
		t/*.m0*
