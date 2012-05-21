#ifndef __M1_COMPILER__
#define __M1_COMPILER__

#include "m1_ast.h"

typedef struct M1_compiler {
	unsigned         errors;
	struct m1_chunk *ast;	
	
} M1_compiler;

#endif

