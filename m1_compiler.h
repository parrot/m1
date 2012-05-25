#ifndef __M1_COMPILER__
#define __M1_COMPILER__

#define NUM_TYPES   4

typedef struct M1_compiler {
	unsigned         		errors;
	struct m1_chunk 	   *ast;	 /* root of the AST */
	int				 		constindex; /* constant table index counter */
	int              		label; /* label generator */
	unsigned    	 		regs[NUM_TYPES]; /* for the register allocator */
	
	struct m1_symboltable  *floats;
	struct m1_symboltable  *ints;
	struct m1_symboltable  *strings;
	struct m1_symboltable  *globals;
	
	struct m1_stack        *breakstack; /* for handling break statements */
	
} M1_compiler;

#endif

