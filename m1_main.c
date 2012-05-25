#include <stdio.h>
#include <stdlib.h>

/* m1parser.h needs to be included /before/ m1lexer.h. */
#include "m1parser.h"

/* prevent declaration of yyparse in m1lexer.h. */
#define YY_DECL
#include "m1lexer.h"

#include "m1_compiler.h"
#include "m1_semcheck.h"
#include "m1_stack.h"
#include "m1_gencode.h"

extern int yyparse(yyscan_t yyscanner, struct M1_compiler * const comp);

int
main(int argc, char *argv[]) {
    FILE        *fp;
    yyscan_t     yyscanner;
    M1_compiler  comp;
    int i;
    
    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        fprintf(stderr, "Could not open file\n");
        exit(EXIT_FAILURE);
    }
   
    /* set up compiler */
   	memset(&comp, 0, sizeof(M1_compiler));
   	
    for(i = 0; i < 4; ++i)
       	comp.regs[i] = 1;
    
    comp.breakstack = new_stack();   	
       	
    /* set up lexer and parser */   	
    yylex_init(&yyscanner);
    yyset_extra(&comp, yyscanner);
    
    yyset_in(fp, yyscanner);

    comp.ints = new_symtab();
    comp.floats = new_symtab();
    comp.globals = new_symtab();
    comp.strings = new_symtab();
    
    yyparse(yyscanner, &comp);
    
    if (comp.errors == 0) {
//    	check(&comp, comp.ast); /*  need to finish */
    	fprintf(stderr, "generating code...\n");
	    gencode(&comp, comp.ast);
    }
    
    fclose(fp);
    return 0;
}

