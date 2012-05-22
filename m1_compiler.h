#ifndef __M1_COMPILER__
#define __M1_COMPILER__



typedef struct M1_compiler {
	unsigned         		errors;
	struct m1_chunk 	   *ast;	
	int				 		constindex;
	int              		label;
	int				 		regs[4];
	struct m1_symboltable  *floats;
	struct m1_symboltable  *ints;
	struct m1_symboltable  *strings;
	
} M1_compiler;

#endif

