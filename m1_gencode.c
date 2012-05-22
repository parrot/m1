/*****************************************

code generation skeleton.

*****************************************/
#include <stdio.h>
#include <stdlib.h>
#include "m1_gencode.h"
#include "m1_ast.h"
#include "m1_compiler.h"

#include "m1_ann.h"

#define OUT	stderr


static m1_reg gencode_expr(M1_compiler *comp, m1_expression *e);

/*

Allocate new registers as needed.

*/
static m1_reg
gen_reg(M1_compiler *comp, data_type type) {
    /* int, num, string, pmc */
    m1_reg reg;
    reg.type = type;
	reg.no   = comp->regs[type]++;   
    return reg;
}

/*

Generate label identifiers.

*/
static int
gen_label(M1_compiler *comp) {
	static int label = 0;
	return label++;	
}


static m1_reg
gencode_number(M1_compiler *comp, double value) {
	/*
	deref Nx, CONSTS, <const_id>
	*/
    m1_reg     reg = gen_reg(comp, 'N');
    m1_symbol *sym = sym_find_num(&floats, value);


    fprintf(OUT, "\tderef\tN%d, CONSTS, %d\n", reg.no, sym->constindex);
    return reg;
}   

static m1_reg
gencode_int(M1_compiler *comp, int value) {
	/*
	deref Ix, CONSTS, <const_id>
	*/
    m1_reg     reg = gen_reg(comp, 'I');
    m1_symbol *sym = sym_find_int(&ints, value);

    fprintf(OUT, "\tderef\tI%d, CONSTS, %d\n", reg.no, sym->constindex);
    return reg;
}

static m1_reg
gencode_string(M1_compiler *comp, NOTNULL(char *value)) {
    m1_reg     reg = gen_reg(comp, 'S');
    m1_symbol *sym = sym_find_str(&strings, value); /* find index of value in CONSTS */
    fprintf(OUT, "\tderef\tS%d, CONSTS, %d\n", reg.no, sym->constindex);
    return reg;
}


static m1_reg
gencode_assign(M1_compiler *comp, NOTNULL(m1_assignment *a)) {
	m1_reg lhs, rhs;
    rhs = gencode_expr(comp, a->rhs);
    lhs = gencode_expr(comp, a->lhs);
    fprintf(OUT, "\tset %c%d, %c%d, x\n", lhs.type, lhs.no, rhs.type, rhs.no);
	return lhs;
}

static m1_reg
gencode_null(M1_compiler *comp) {
	m1_reg reg;
/*    fprintf(OUT, "null");    
*/
    return reg;
}   

static m1_reg
gencode_obj(M1_compiler *comp, m1_object *obj) {
	m1_reg reg, oreg;
	
	reg  = gen_reg(comp, 'I');
	oreg = gen_reg(comp, 'I');
	
	
    switch (obj->type) {
        case OBJECT_MAIN:
        	
            fprintf(OUT, "\tset\tI%d, I%d\n", 1000, 2000);
            break;
        case OBJECT_FIELD:
            fprintf(OUT, "\tset_imm\tI%d, %d\n", 1000, 4);
            break;
        case OBJECT_DEREF:
	/* todo */
            break;
        case OBJECT_INDEX:         
            reg = gencode_expr(comp, obj->obj.index);
            break;            
        default:
            break;
    }      
    
    if (obj->next) {
        reg = gencode_obj(comp, obj->next);   
    }
    
    return reg;
}

static m1_reg
gencode_while(M1_compiler *comp, m1_whileexpr *w) {
	/*
	
	START:
	<code for cond>
	goto_if END
	<code for block>
	goto START
	END:
	*/
	m1_reg reg;
	int endlabel   = gen_label(comp), 
	    startlabel = gen_label(comp);
	
	fprintf(OUT, "L%d:\n", startlabel);
    reg = gencode_expr(comp, w->cond);
	fprintf(OUT, "\tgoto_if\n");
    gencode_expr(comp, w->block);
    fprintf(OUT, "\tgoto \tL%d\n", startlabel);
	fprintf(OUT, "L%d:\n", endlabel);
	
	return reg;
}

static m1_reg
gencode_dowhile(M1_compiler *comp, m1_whileexpr *w) {
	/*
	START:
	<code for block>
	<code for cond>
	goto_if START
	*/
    m1_reg reg;
    int    startlabel = gen_label(comp);
    
    fprintf(OUT, "L%d:\n", startlabel);
    gencode_expr(comp, w->block);
    
    reg = gencode_expr(comp, w->cond);
    fprintf(OUT, "\tgoto_if\tL%d\n", startlabel);
    
    return reg;
}

static m1_reg
gencode_for(M1_compiler *comp, m1_forexpr *i) {
	/*
	
	<code for init>
	START:
	<code for cond>
	goto_if END
	<code for block>
	<code for step>
	goto START
	END:
	*/
    int startlabel = gen_label(comp),
        endlabel   = gen_label(comp);
        
    m1_reg reg;
    
    if (i->init)
        gencode_expr(comp, i->init);

	fprintf(OUT, "L%d\n", startlabel);
	
    if (i->cond)
        reg = gencode_expr(comp, i->cond);
   
    fprintf(OUT, "\tgoto_if L%d\n", endlabel);
    
    if (i->block) 
        gencode_expr(comp, i->block);
        
    if (i->step)
        gencode_expr(comp, i->step);
    
    fprintf(OUT, "\tgoto L%d\n", startlabel);
    fprintf(OUT, "L%d:\n", endlabel);
    
    
    return reg;
}

static m1_reg 
gencode_if(M1_compiler *comp, m1_ifexpr *i) {
	/*
	
	result1 = <evaluate condition>
	goto_if result1 == 0, ELSE
	<code for ifblock>
	goto END
	ELSE:
	<code for elseblock>
	END:
	*/
	m1_reg cond;
	int endlabel  = gen_label(comp),
		elselabel = gen_label(comp) ;

	
    cond = gencode_expr(comp, i->cond);
	
	fprintf(OUT, "\tgoto_if\tL_IF_%d\n", elselabel);
	
    gencode_expr(comp, i->ifblock);
	fprintf(OUT, "\tgoto L%d\n", endlabel);
	fprintf(OUT, "L_IF_%d:\n", elselabel);
	
    if (i->elseblock) {    	
        gencode_expr(comp, i->elseblock);
    }
    fprintf(OUT, "L_IF_%d:\n", endlabel);
    return cond;       
}

static m1_reg
gencode_deref(M1_compiler *comp, m1_object *o) {
	m1_reg reg;
/*    fprintf(OUT, "*");*/
    reg = gencode_obj(comp, o);
    return reg;
}

static m1_reg
gencode_address(M1_compiler *comp, m1_object *o) {
	m1_reg reg;
/*    fprintf(OUT, "&"); */
    reg = gencode_obj(comp, o);   
    return reg;
}

static m1_reg
gencode_return(M1_compiler *comp, m1_expression *e) {
	m1_reg reg;
    reg = gencode_expr(comp, e);
    return reg;
}

static m1_reg
gencode_or(M1_compiler *comp, m1_binexpr *b) {
	/*
	result1 = <evaluate left>
	goto_if result1 != 0, END
	result2 = <evaluate right>
	END:
	*/
	m1_reg left, right;
	int endlabel;
	
	endlabel = gen_label(comp);
	left     = gencode_expr(comp, b->left);
	
	fprintf(OUT, "\tgoto_if L_OR_%d\n", endlabel);
	
	right = gencode_expr(comp, b->right);
	
	fprintf(OUT, "L_OR_%d:\n", endlabel);
	return left;	
}

static m1_reg
gencode_and(M1_compiler *comp, m1_binexpr *b) {
	/*
	result1 = <evaluate left>
	goto_if result1 == 0, END
	result2 = <evaluate right>
	END:
	*/
	m1_reg left, right;
	int endlabel = gen_label(comp);
	
	left = gencode_expr(comp, b->left);
	fprintf(OUT, "\tgoto_if\tL_AND_%d\n", endlabel);	
	right = gencode_expr(comp, b->right);
	
	fprintf(OUT, "L_AND_%d:\n", endlabel);
	return right;
}

static m1_reg
gencode_binary(M1_compiler *comp, m1_binexpr *b) {
    char  *op = NULL;
    m1_reg left, 
    	   right,
    	   target;
    
    switch(b->op) {
    	case OP_ASSIGN:
    		op = "set"; /* in case of a = b = c; then b = c part is a binary expression */
    		break;
        case OP_PLUS:
            op = "add";
            break;
        case OP_MINUS:
            op = "sub";
            break;
        case OP_MUL:
            op = "mult";
            break;
        case OP_DIV:
            op = "div";
            break;
        case OP_MOD:
            op = "mod";
            break;
        case OP_XOR:
            op = "xor";
            break;
        case OP_GT:
/*            op = ">";*/
            break;
        case OP_GE:
/*            op = ">=";*/
            break;
        case OP_LT:
/*            op = "<";*/
            break;
        case OP_LE:
/*            op = "<=";*/
            break;
        case OP_EQ:
/*            op = "==";*/
            break;
        case OP_NE:
/*            op = "!=";*/
            break;
        case OP_AND: /* a && b */
            return gencode_and(comp, b);

        case OP_OR: /* a || b */
            return gencode_or(comp, b);

        case OP_BAND:
            op = "and";
            break;
        case OP_BOR:
            op = "or";
            break;
        default:
            op = "unknown op";
            break;   
    }
    
    left        = gencode_expr(comp, b->left);
    right       = gencode_expr(comp, b->right);  
    target      = gen_reg(comp, left.type);  
    
    fprintf(OUT, "\t%s\t%c%d, %c%d, %c%d\n", op, target.type, target.no, 
           left.type, left.no, right.type, right.no);
    return target;
}



static m1_reg
gencode_unary(M1_compiler *comp, NOTNULL(m1_unexpr *u)) {
    char  *op;
    int    postfix = 0;
    m1_reg reg;
    m1_reg target = gen_reg(comp, 'I'); /* for final value */
    m1_reg one    = gen_reg(comp, 'I'); /* to store "1" */
    
    switch (u->op) {
        case UNOP_POSTINC:
        case UNOP_POSTDEC:
            postfix = 1;
            break;
        case UNOP_PREINC:
        case UNOP_PREDEC:
            postfix = 0; 
            break;
        case UNOP_MINUS:
            postfix = 0;
            op = "-"; /* TODO */
            break;    
        default:
            op = "unknown op";
            break;   
    }   
    /* generate code for the expression */ 
    reg = gencode_expr(comp, u->expr);
    
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n", one.no);
    fprintf(OUT, "\tadd_i\tI%d, I%d, I%d\n", target.no, reg.no, one.no);
    
    if (postfix == 0) { 
        /* prefix; return reg containing value before adding 1 */
    	reg.no = target.no; 
    }	
    
   	return reg;	    
    
}

static m1_reg
gencode_break(M1_compiler *comp) {
	m1_reg reg;
    fprintf(OUT, "\tgoto\tL??\n");
    return reg;
}

static m1_reg
gencode_funcall(M1_compiler *comp, m1_funcall *f) {
	m1_reg reg;
    /*fprintf(OUT, "%s();", f->name);   */
    fprintf(OUT, "\tgoto_chunk\n");
    return reg;
}

static m1_reg
gencode_print(M1_compiler *comp, m1_expression *expr) {
    m1_reg reg;
    reg = gencode_expr(comp, expr);
	fprintf(OUT, "\tprint_%c\tI0, %c%d, x\n", reg.type, reg.type, reg.no);
	return reg;
}

static m1_reg
gencode_expr(M1_compiler *comp, m1_expression *e) {

    m1_reg reg;
    if (e == NULL) {
    	
        return reg;
    }
        
    switch (e->type) {
        case EXPR_NUMBER:
            reg = gencode_number(comp, e->expr.floatval);
            break;
        case EXPR_INT:
            reg = gencode_int(comp, e->expr.intval);
            break;
        case EXPR_STRING:
            reg = gencode_string(comp, e->expr.str);     
            break;
        case EXPR_BINARY:
            reg = gencode_binary(comp, e->expr.b);
            break;
        case EXPR_UNARY:
            reg = gencode_unary(comp, e->expr.u);
            break;
        case EXPR_FUNCALL:
            reg = gencode_funcall(comp, e->expr.f);
            break;
        case EXPR_ASSIGN:
            reg = gencode_assign(comp, e->expr.a);
            break;
        case EXPR_IF:   
            reg = gencode_if(comp, e->expr.i);
            break;
        case EXPR_WHILE:
            reg = gencode_while(comp, e->expr.w);
            break;
        case EXPR_DOWHILE:
            reg = gencode_dowhile(comp, e->expr.w);
            break;
        case EXPR_FOR:
            reg = gencode_for(comp, e->expr.o);
            break;
        case EXPR_RETURN:
            reg = gencode_return(comp, e->expr.e);
            break;
        case EXPR_NULL:
            reg = gencode_null(comp);
            break;
        case EXPR_DEREF:
            reg = gencode_deref(comp, e->expr.t);
            break;
        case EXPR_ADDRESS:
            reg = gencode_address(comp, e->expr.t);
            break;
        case EXPR_OBJECT:
            reg = gencode_obj(comp, e->expr.t);
            break;
        case EXPR_BREAK:
            reg = gencode_break(comp);
            break;
        case EXPR_CONSTDECL:
        	break;
        case EXPR_VARDECL:
            break;
        case EXPR_PRINT:
            gencode_print(comp, e->expr.e);   
            break; 
        default:
            fprintf(stderr, "unknown expr type");   
            exit(EXIT_FAILURE);
    }   
    return reg;
}

static void
print_consts(NOTNULL(m1_symboltable *table)) {
	m1_symbol *iter;
	iter = table->syms;
	while (iter != NULL) {
		
		switch (iter->type) {
			case VAL_STRING:
				fprintf(OUT, "%d %s\n", iter->constindex, iter->value.str);
				break;
			case VAL_FLOAT:
				fprintf(OUT, "%d %f\n", iter->constindex, iter->value.fval);
				break;
			case VAL_INT:
				fprintf(OUT, "%d %d\n", iter->constindex, iter->value.ival);
				break;
			default:
				fprintf(stderr, "unknown symbol type");
				exit(EXIT_FAILURE);
		}
		iter = iter->next;	
	}
}
static void
gencode_consts(M1_compiler *comp) {
	fprintf(OUT, ".constants\n");
	print_consts(&strings);	
	print_consts(&floats);	
	print_consts(&ints);	
}

static void
gencode_metadata(M1_compiler *comp) {
	fprintf(OUT, ".metadata\n");	
}

static void 
gencode_chunk(M1_compiler *comp, m1_chunk *c) {
    m1_expression *iter;
    
    fprintf(OUT, ".chunk \"%s\"\n", c->name);
    
    gencode_consts(comp);
    gencode_metadata(comp);
    
    fprintf(OUT, ".bytecode\n");
    
    /* generate code for statements */
    iter = c->block;
    while (iter != NULL) {
        (void)gencode_expr(comp, iter);
        iter = iter->next;
    }
}

void 
gencode(M1_compiler *comp, m1_chunk *ast) {
    m1_chunk *iter = ast;
     
    fprintf(OUT, ".version 0\n");
    while (iter != NULL) {        
        gencode_chunk(comp, iter);
        iter = iter->next;   
    }
}



