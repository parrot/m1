#include <stdio.h>
#include <stdlib.h>
#include "m1_semcheck.h"


static void
check_expr(M1_compiler *comp, m1_expression *chunk) {
	
}


static data_type
check_chunk(M1_compiler *comp, m1_chunk *chunk) {
	
	m1_expression *iter = chunk->block;
	
	while (iter != NULL) {
		check_expr(comp, iter);
		iter = iter->next;	
	}
	return chunk->rettype;
}

void 
check(M1_compiler *comp, m1_chunk *ast) {
	m1_chunk *iter = ast;
	
	while (iter != NULL) {
		check_chunk(comp, iter);
		iter = iter->next;	
	}	
}
