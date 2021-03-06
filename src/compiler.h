
#ifndef __M1_COMPILER__
#define __M1_COMPILER__

#define NUM_TYPES       4

#define REG_TYPE_NUM    4
#define REG_NUM         61

#include <stdio.h>


/* needed for declaring yyscan_t as a member of compiler struct below. */
#ifndef YY_TYPEDEF_YY_SCANNER_T
#  define YY_TYPEDEF_YY_SCANNER_T

typedef void * yyscan_t;

#endif

/* compiler struct that is passed around to ALL functions. */
typedef struct M1_compiler {
    char                  *current_filename;
    char                  *current_namespace;
	unsigned int           errors;
	unsigned int           warnings;
	
	struct m1_chunk       *ast;	    /* root of the AST */
	unsigned int           constindex; /* constant table index counter */
	unsigned int           label;      /* label generator */
	unsigned int  	       regs[NUM_TYPES]; /* for the register allocator */
	                       
    char                  *parsingtype; /* when parsing var declarations, need to know this when entering symbols. */
    
	struct m1_intstack    *breakstack; /* for handling break statements */
	struct m1_intstack    *continuestack; /* for handling continue statements */
	
    struct m1_chunk       *currentchunk; /* current chunk being parsed, if any. */
	struct m1_type        *declarations;  /* list of declarations (eg structs) */

	struct m1_regstack    *regstack; /* for storing registers in code generator */		
	yyscan_t               yyscanner; /* pointer to the lexer structure */
		
	struct m1_symboltable *currentsymtab; /* a pointer to the current symbol table, if any. */
	
	struct m1_symboltable *globalsymtab; /* to store function names */
	
	unsigned int           enum_const_counter; /* for parsing enums that don't specify values. */
	
	char                   registers[REG_TYPE_NUM][REG_NUM]; /* register allocation system. */
	
	int                    no_reg_opt; /* command-line option to turn off register allocator. */
	
	/* code generator fields. */
	FILE                  *outfile;
	struct m0_instr       *lastgenerated;
	struct m0_chunk       *current_m0chunk;
	
} M1_compiler;

#endif

