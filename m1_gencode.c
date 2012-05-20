/*****************************************

code generation skeleton.

*****************************************/
#include <stdio.h>
#include <stdlib.h>
#include "m1_gencode.h"
#include "m1_ast.h"
#define OUT	stderr



static int
gen_reg(data_type type) {
    /* int, num, string, pmc */
    static int regs[4] = {1, 1, 1, 1};
    return regs[type]++;   
}


static m1_reg gencode_expr(m1_expression *e);

static m1_reg
gencode_number(double value) {
    int valreg;
    m1_reg reg;
    reg.no = gen_reg(TYPE_NUM);
    reg.type = 'N';

    fprintf(OUT, "deref\tN%d, CONSTS, %d\n", reg.no, valreg);
    return reg;
}   

static m1_reg
gencode_int(int value) {
    m1_reg reg;
    reg.no = gen_reg(TYPE_INT);
    reg.type = 'I';
    fprintf(OUT, "deref\tI%d, CONSTS, %d\n", reg.no, value);
    return reg;
}

static m1_reg
gencode_string(char *value) {
    m1_reg reg;
    int idx;
    m1_symbol *sym = sym_find_str(&strings, value); /* find index of value in CONSTS */
    idx = sym->constindex;
    reg.no = gen_reg(TYPE_STRING);
    reg.type = 'S';

    fprintf(OUT, "deref\tS%d, CONSTS, %d\n", reg.no, idx);
    return reg;
}


static void
gencode_assign(m1_assignment *a) {
    gencode_expr(a->rhs);
    gencode_expr(a->lhs);

}

static void
gencode_null(void) {
    fprintf(OUT, "null");    
}   

static void
gencode_obj(m1_object *obj) {
	return;
	/*
    switch (obj->type) {
        case OBJECT_MAIN:
            fprintf(OUT, "%s", obj->obj.field);
            break;
        case OBJECT_FIELD:
            fprintf(OUT, ".%s", obj->obj.field);
            break;
        case OBJECT_DEREF:
            fprintf(OUT, "->%s", obj->obj.field);
            break;
        case OBJECT_INDEX:
            fprintf(OUT, "[");
            gencode_expr(obj->obj.index);
            fprintf(OUT, "]");
            break;            
        default:
            break;
    }      
    
    if (obj->next) {
        gencode_obj(obj->next);   
    }
    */
}

static void
gencode_while(m1_whileexpr *w) {

    gencode_expr(w->cond);

    gencode_expr(w->block);

}

static void
gencode_dowhile(m1_whileexpr *w) {
    
    gencode_expr(w->block);
    
    
    gencode_expr(w->cond);
    
}

static void
gencode_for(m1_forexpr *i) {
   
    if (i->init)
        gencode_expr(i->init);

    if (i->cond)
        gencode_expr(i->cond);
   
    if (i->step)
        gencode_expr(i->step);
   
    if (i->block)
        gencode_expr(i->block);
   
}

static void
gencode_if(m1_ifexpr *i) {

    gencode_expr(i->cond);

    gencode_expr(i->ifblock);

    if (i->elseblock) {

        gencode_expr(i->elseblock);

    }
           
}

static void
gencode_deref(m1_object *o) {
    fprintf(OUT, "*");
    gencode_obj(o);
}

static void
gencode_address(m1_object *o) {
    fprintf(OUT, "&");
    gencode_obj(o);   
}

static void
gencode_return(m1_expression *e) {
	m1_reg reg;
	
    reg = gencode_expr(e);
}

static m1_reg
gencode_binary(m1_binexpr *b) {
    char *op;
    m1_reg left, right; 
    m1_reg target;
    
    switch(b->op) {
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
        case OP_EXP:
            op = "^";
            break;
        case OP_GT:
            op = ">";
            break;
        case OP_GE:
            op = ">=";
            break;
        case OP_LT:
            op = "<";
            break;
        case OP_LE:
            op = "<=";
            break;
        case OP_EQ:
            op = "==";
            break;
        case OP_NE:
            op = "!=";
            break;
        case OP_AND:
            op = "&&";
            break;
        case OP_OR:
            op = "||";
            break;
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
    
    left  = gencode_expr(b->left);
    right = gencode_expr(b->right);  
    target.no = gen_reg(left.type);  
    target.type = left.type;
    fprintf(OUT, "%s\t%c%d, %c%d, %c%d\n", op, target.type, target.no, left.type, left.no, right.type, right.no);
    return target;
}

static m1_reg
gencode_unary(m1_unexpr *u) {
    char *op;
    int postfix = 0;
    m1_reg reg;
    int target = gen_reg(TYPE_INT);
    int one    = gen_reg(TYPE_INT);
    
    switch (u->op) {
        case UNOP_POSTINC:
            postfix = 1;
            op = "++";
            break;
        case UNOP_PREINC:
            postfix = 0;
            op = "++";
            break;
        case UNOP_POSTDEC:
            postfix = 1;
            op = "--";
            break;
        case UNOP_PREDEC:
            postfix = 0; 
            op = "--";
            break;
        case UNOP_MINUS:
            postfix = 0;
            op = "-";
            break;    
        default:
            op = "unknown op";
            break;   
    }    
    reg = gencode_expr(u->expr);
    fprintf(OUT, "set_imm\tI%d, 0, 1\n", one);
    fprintf(OUT, "add_i\tI%d, I%d, I%d\n", target, reg.no, one);
    return reg;	    
}

static void
gencode_break(void) {
    fprintf(OUT, "goto\t??\n");
}

static void
gencode_funcall(m1_funcall *f) {
    fprintf(OUT, "%s();", f->name);   
}

static void
gencode_print(m1_expression *expr) {
    m1_reg reg;
    reg = gencode_expr(expr);
	fprintf(OUT, "print_%c\tI0, %c%d, x\n", reg.type, reg.type, reg.no);
}

static m1_reg
gencode_expr(m1_expression *e) {

    m1_reg reg;
    if (e == NULL) {
    	fprintf(stderr, "expr is NULL");	 
        return reg;
    }
        
    switch (e->type) {
        case EXPR_NUMBER:
            reg = gencode_number(e->expr.floatval);
            break;
        case EXPR_INT:
            reg = gencode_int(e->expr.intval);
            break;
        case EXPR_STRING:
            reg = gencode_string(e->expr.str);     
            break;
        case EXPR_BINARY:
            reg = gencode_binary(e->expr.b);
            break;
        case EXPR_UNARY:
            reg = gencode_unary(e->expr.u);
            break;
        case EXPR_FUNCALL:
            gencode_funcall(e->expr.f);
            break;
        case EXPR_ASSIGN:
            gencode_assign(e->expr.a);
            break;
        case EXPR_IF:   
            gencode_if(e->expr.i);
            break;
        case EXPR_WHILE:
            gencode_while(e->expr.w);
            break;
        case EXPR_DOWHILE:
            gencode_dowhile(e->expr.w);
            break;
        case EXPR_FOR:
            gencode_for(e->expr.o);
            break;
        case EXPR_RETURN:
            gencode_return(e->expr.e);
            break;
        case EXPR_NULL:
            gencode_null();
            break;
        case EXPR_DEREF:
            gencode_deref(e->expr.t);
            break;
        case EXPR_ADDRESS:
            gencode_address(e->expr.t);
            break;
        case EXPR_OBJECT:
            gencode_obj(e->expr.t);
            break;
        case EXPR_BREAK:
            gencode_break();
            break;
        case EXPR_CONSTDECL:
        case EXPR_VARDECL:
            break;
        case EXPR_PRINT:
            gencode_print(e->expr.e);   
            break; 
        default:
            fprintf(stderr, "unknown expr type");   
            exit(EXIT_FAILURE);
    }   
    return reg;
}

static void
print_consts(m1_symboltable *table) {
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
				break;
		}
		iter = iter->next;	
	}
}
static void
gencode_consts(void) {
	fprintf(OUT, ".constants\n");
	print_consts(&strings);	
	print_consts(&floats);	
	print_consts(&ints);	
}

static void
gencode_metadata(void) {
	fprintf(OUT, ".metadata\n");	
}

static void 
gencode_chunk(m1_chunk *c) {
    m1_expression *iter = c->block;
    fprintf(OUT, ".chunk \"%s\"\n", c->name);
    
    gencode_consts();
    gencode_metadata();
    fprintf(OUT, ".bytecode\n");
    
    while (iter != NULL) {
        gencode_expr(iter);
        iter = iter->next;
    }
}

void 
gencode(m1_chunk *ast) {
    m1_chunk *iter = ast;
     
    while (iter != NULL) {        
        gencode_chunk(iter);
        iter = iter->next;   
    }
}



