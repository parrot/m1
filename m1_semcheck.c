#include <stdio.h>
#include <stdlib.h>
#include "m1_symtab.h"
#include "m1_semcheck.h"
#include "m1_ast.h"

static m1_valuetype
check_object(M1_compiler *comp, m1_object *obj) {
	switch (obj->type) {
		case OBJECT_MAIN: /* x in x.y */
		  /* find obj in symbol table */
		  break;
		
		case OBJECT_FIELD:
		case OBJECT_INDEX:
		case OBJECT_DEREF:
			
/*		case OBJECT_SCOPE:*/
		default:
			exit(EXIT_FAILURE);
	}	
	return 0;
}

static m1_valuetype
check_expr(M1_compiler *comp, m1_expression *expr) {
	m1_valuetype t;
	switch (expr->type) {
		case EXPR_NUMBER:
			return VAL_FLOAT;
		case EXPR_INT:
			return VAL_INT;
		case EXPR_BINARY: 
		{
			m1_valuetype ltype = check_expr(comp, expr->expr.b->left);
			m1_valuetype rtype = check_expr(comp, expr->expr.b->right);
			
			if (ltype != rtype) {
				fprintf(stderr, "Incompatible types in binary expression\n");
				++comp->errors;
			}			
			return ltype;
		}
		case EXPR_UNARY:
    	case EXPR_FUNCALL:
    	case EXPR_ASSIGN:
    	case EXPR_IF:
    	case EXPR_WHILE:
    	case EXPR_DOWHILE:
    	case EXPR_FOR:
    	case EXPR_RETURN:
    	case EXPR_NULL:
    	case EXPR_DEREF:
    	case EXPR_ADDRESS:
		case EXPR_OBJECT:
			return check_object(comp, expr->expr.t);
    	case EXPR_BREAK:
    		break;
	    case EXPR_STRING:
	    	return VAL_STRING;
	    	
		case EXPR_CONSTDECL:
   		case EXPR_VARDECL:
		case EXPR_M0BLOCK:
    	case EXPR_PRINT:
    		break;
		default:
			break;	
	}
	return t;
}


static m1_valuetype
check_chunk(M1_compiler *comp, m1_chunk *chunk) {
	m1_valuetype type;
	m1_expression *iter = chunk->block;
	
	while (iter != NULL) {
		type = check_expr(comp, iter);
		if (iter->type == EXPR_RETURN) {
			if (chunk->rettype != type) {
				fprintf(stderr, "Return type of function '%s' does not match type of returned expression\n", chunk->name);
				++comp->errors;	
			}
		}
		iter = iter->next;	
	}

	return chunk->rettype;
}

void 
check(M1_compiler *comp, m1_chunk *ast) {
	m1_chunk *iter = ast;
	
	while (iter != NULL) {
		(void)check_chunk(comp, iter);
		iter = iter->next;	
	}	
}
