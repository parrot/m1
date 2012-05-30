#ifndef __M1_COMPILER__
#define __M1_COMPILER__

#define NUM_TYPES   4

typedef struct M1_compiler {
	unsigned          errors;
	struct m1_chunk  *ast;	 /* root of the AST */
	int				  constindex; /* constant table index counter */
	int               label; /* label generator */
	int          	  regs[NUM_TYPES]; /* for the register allocator */
	int               expect_usertype; /* identifiers can be types or identifiers. 
	                       Keep track what the lexer should return (TK_IDENT or TK_USERTYPE 	
	                       */
    int               parsingtype; /* when parsing var declarations, need to know this when entering symbols. */
	struct m1_stack  *breakstack; /* for handling break statements */
	
    struct m1_chunk  *currentchunk; /* current chunk being parsed, if any. */
    
    struct m1_struct *structs; /* list of struct definitions. */
	
	struct m1_decl   *declarations; 
	
	int               is_parsing_usertype; /* boolean to indicate whether a type is parsed. */
	
} M1_compiler;

#endif

