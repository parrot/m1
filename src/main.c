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
#include "decl.h"

#include <assert.h>

extern int yyparse(yyscan_t yyscanner, struct M1_compiler * const comp);


static void
init_compiler(M1_compiler *comp) {
   	memset(comp, 0, sizeof(M1_compiler)); 
   	
    comp->breakstack      = new_intstack();   
    comp->regstack        = new_regstack();	   
    comp->continuestack   = new_intstack();   
    comp->expect_usertype = 0; /* when not parsing a function's body, 
                                   then identifiers are types */   	
    comp->is_parsing_usertype = 1;
    
    /* register built-in types in type declaration module. */
    type_enter_type(comp, "void", DECL_VOID, 0);
    type_enter_type(comp, "int", DECL_INT, 4);
    type_enter_type(comp, "num", DECL_NUM, 8);
    type_enter_type(comp, "bool", DECL_BOOL, 4); /* bools are stored in ints. */
    type_enter_type(comp, "string", DECL_STRING, 4);  /* strings are pointers, so size is 4. */
    type_enter_type(comp, "char", DECL_CHAR, 4); /* XXX can this be 1? what about padding in structs? */
    
    /* global symbol table for functions, as they need a return type m1_type pointer. */
    comp->globalsymtab = new_symtab();
}

int
main(int argc, char *argv[]) {
    FILE        *fp;
    yyscan_t     yyscanner;
    M1_compiler  comp;
    int          turnoff_reg_opt = 0;
    char        *outputfile = "a.m1";
    
    if (argc <= 1) {
        fprintf(stderr, "Usage: m1 <file>\n");
        exit(EXIT_FAILURE);    
    }
    
    if (strcmp(argv[1], "-r") == 0) {
        /* turn of register optimization. */
        turnoff_reg_opt = 1;   
        argv++; /* go to next arg. */
    }
    
    if (strcmp(argv[1], "-o") == 0) {
        argv++;
        outputfile = argv[1];
        argv++;   
    }
    
    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        fprintf(stderr, "Could not open file\n");
        exit(EXIT_FAILURE);
    }
   
    /* set up compiler */
    init_compiler(&comp);
    comp.no_reg_opt       = turnoff_reg_opt;
    comp.current_filename = argv[1];
                                       
    /* set up lexer and parser */   	
    yylex_init(&yyscanner);    
    yyset_extra(&comp, yyscanner); 
    yyset_in(fp, yyscanner);
    
    comp.yyscanner = yyscanner; /* yyscanner has a pointer to comp, and vice versa. */
    
    yyparse(yyscanner, &comp);
    
    fprintf(stderr, "parsing done\n");
    if (comp.errors == 0) 
    {
        assert(intstack_isempty(comp.breakstack) != 0);
        assert(intstack_isempty(comp.continuestack) != 0);
        
    	check(&comp, comp.ast); /*  need to finish */
    	if (comp.errors == 0) 
    	{
        	fprintf(stderr, "generating code...\n");
        	comp.outfile = fopen(outputfile, "w");
	        gencode(&comp, comp.ast);
	        fclose(comp.outfile);
    	}
    	else {
    	   fprintf(stderr, "%d errors and %d warnings\n", comp.errors, comp.warnings);
    	}
    }
    
    fclose(fp);
    fprintf(stderr, "compilation done\n");
    return 0;
}

