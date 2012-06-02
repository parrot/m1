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
	src/m1_ast$(O) \
	src/m1parser$(O) \
	src/m1lexer$(O) \
	src/m1_symtab$(O) \
	src/m1_semcheck$(O) \
	src/m1_stack$(O) \
	src/m1_decl$(O) \
	src/m1_eval$(O) \
	src/m1_instr$(O) \
	src/m1_gencode$(O) \
	src/m1_main$(O) \

m1: $(M1_O_FILES)
	$(CC) -I$(@D) -o m1 $(M1_O_FILES)

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
	
src/m1_ast$(O): src/m1_ast.c src/m1_ast.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_ast.c

src/m1_eval$(O): src/m1_eval.c src/m1_eval.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_eval.c	

src/m1_symtab$(O): src/m1_symtab.c src/m1_symtab.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_symtab.c

src/m1_instr$(O): src/m1_instr.c src/m1_instr.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_instr.c

src/m1_gencode$(O): src/m1_gencode.c src/m1_gencode.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_gencode.c

src/m1_semcheck$(O): src/m1_semcheck.c src/m1_semcheck.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_semcheck.c

src/m1_stack$(O): src/m1_stack.c src/m1_stack.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_stack.c

src/m1_main$(O): src/m1parser.h src/m1_main.c
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_main.c

m1_decl$(O): src/m1_decl.c src/m1_decl.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1_decl.c

clean:
	$(RM) -rf src/m1parser.* \
		src/m1lexer.* \
		src/*$(O) \
		./m1$(EXE)
