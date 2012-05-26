#ifndef __M1_COMPILER__
#define __M1_COMPILER__

#define NUM_TYPES   4

typedef struct M1_compiler {
	unsigned         		errors;
	struct m1_chunk 	   *ast;	 /* root of the AST */
	int				 		constindex; /* constant table index counter */
	int              		label; /* label generator */
	int          	 		regs[NUM_TYPES]; /* for the register allocator */
		
	struct m1_stack        *breakstack; /* for handling break statements */
	
    struct m1_chunk        *currentchunk; /* current chunk being parsed, if any. */
	
} M1_compiler;

#endif

