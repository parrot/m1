#include <stdio.h>
#include <stdlib.h>

/* m1parser.h needs to be included /before/ m1lexer.h. */
#include "m1parser.h"

/* prevent declaration of yyparse in m1lexer.h. */
#define YY_DECL
#include "m1lexer.h"

#include "compiler.h"
#include "semcheck.h"
#include "stack.h"
#include "gencode.h"

#include <assert.h>

extern int yyparse(yyscan_t yyscanner, struct M1_compiler * const comp);

int
main(int argc, char *argv[]) {
    FILE        *fp;
    yyscan_t     yyscanner;
    M1_compiler  comp;
    
    if (argc <= 1) {
        fprintf(stderr, "Usage: m1 <file>\n");
        exit(EXIT_FAILURE);    
    }
    
    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        fprintf(stderr, "Could not open file\n");
        exit(EXIT_FAILURE);
    }
   
    /* set up compiler */
   	memset(&comp, 0, sizeof(M1_compiler)); 
    comp.breakstack = new_intstack();   
    comp.regstack   = new_regstack();	
    
    comp.expect_usertype = 0; /* when not parsing a function's body, 
                                   then identifiers are types */   	
    comp.is_parsing_usertype = 1;
                                       
    /* set up lexer and parser */   	
    yylex_init(&yyscanner);    
    yyset_extra(&comp, yyscanner); 
    yyset_in(fp, yyscanner);
    
    comp.yyscanner = yyscanner; /* yyscanner has a pointer to comp, and vice versa. */
    
    yyparse(yyscanner, &comp);
    
    fprintf(stderr, "parsing done\n");
    if (comp.errors == 0) 
    {
    	check(&comp, comp.ast); /*  need to finish */
    	//if (comp.errors == 0) 
    	{
        	fprintf(stderr, "generating code...\n");
	        gencode(&comp, comp.ast);
    	}
    }
    
    fclose(fp);
    fprintf(stderr, "compilation done\n");
    return 0;
}

