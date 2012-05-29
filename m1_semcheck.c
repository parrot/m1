#include <stdio.h>
#include <stdlib.h>
#include "m1_symtab.h"
#include "m1_semcheck.h"
#include "m1_ast.h"

#include <assert.h>


static m1_type check_expr(M1_compiler *comp, m1_expression *e);

static void
type_error(M1_compiler *comp, unsigned line, char *msg) {
    fprintf(stderr, "Error (line %d): %s\n", line, msg);
    ++comp->errors;   
}

static void
warning(M1_compiler *comp, unsigned line, char *msg) {
    fprintf(stderr, "Warning (line %d): %s\n", line, msg);   
}

static m1_type
check_number(M1_compiler *comp, double value) {
    return TYPE_NUM;
}   

static m1_type
check_int(M1_compiler *comp, int value) {
    return TYPE_INT;
}

static m1_type
check_string(M1_compiler *comp, char *str) {
    return TYPE_STRING;
}

static m1_type
check_true(M1_compiler *comp) {
    return TYPE_BOOL;   
}

static void
check_assign(M1_compiler *comp, m1_assignment *a) {
    m1_type ltype = check_expr(comp, a->lhs);
    m1_type rtype = check_expr(comp, a->rhs);
    
    if (ltype != rtype) {
        type_error(comp, 0, 
                   "type of left expression does not match type "
                   "of right expression in assignment");   
    }
    switch (ltype) {
        case TYPE_VOID:
            
        case TYPE_INT:
        case TYPE_BOOL:
        case TYPE_NUM:
        case TYPE_STRING:  
        case TYPE_CHUNK:
        case TYPE_NULL:
            break;
    }
}

static m1_type
check_null(M1_compiler *comp) { 
    return TYPE_NULL;  
}   

static m1_type
check_obj(M1_compiler *comp, m1_object *obj) {
    m1_type t;
    if (obj->type == OBJECT_LINK) {
        
        t = check_obj(comp, obj->parent);   
        
    }
    
    switch (obj->type) {
        case OBJECT_MAIN:

            break;
        case OBJECT_FIELD:

            break;
        case OBJECT_DEREF:

            break;
        case OBJECT_INDEX: {
            m1_type t;
            t = check_expr(comp, obj->obj.index);
            if (t != TYPE_INT) {
                type_error(comp, obj->line, "result of expression does not yield an integer value!");   
            }

            break;          
        }  
        default:
            break;
    }      
    return t;
    
}

static void
check_while(M1_compiler *comp, m1_whileexpr *w) {

    m1_type condtype = check_expr(comp, w->cond);
    
    if (condtype != TYPE_BOOL) {
           
    }

    check_expr(comp, w->cond);

}

static void
check_dowhile(M1_compiler *comp, m1_whileexpr *w) {

    check_expr(comp, w->block);


    check_expr(comp, w->cond);

}

static void
check_for(M1_compiler *comp, m1_forexpr *i) {

    if (i->init)
        check_expr(comp, i->init);

    if (i->cond)
        check_expr(comp, i->cond);

    if (i->step)
        check_expr(comp, i->step);

    if (i->block)
        check_expr(comp, i->block);

}

static void
check_if(M1_compiler *comp, m1_ifexpr *i) {

    m1_type condtype = check_expr(comp, i->cond);
    if (condtype != TYPE_BOOL) {
        type_error(comp, 0, "condition in if-statement does not yield boolean value");   
    }

    check_expr(comp, i->ifblock);

    if (i->elseblock) {
        check_expr(comp, i->elseblock);
    }
           
}

static m1_type
check_deref(M1_compiler *comp, m1_object *o) {
    m1_type t = check_obj(comp, o);
    return t;
}

static m1_type
check_address(M1_compiler *comp, m1_object *o) {
    m1_type t = check_obj(comp, o);   
    return t;
}

static m1_type
check_return(M1_compiler *comp, m1_expression *e) {
    return check_expr(comp, e);
}

static void
check_binary(M1_compiler *comp, m1_binexpr *b) {
    char *op;

    switch(b->op) {
        case OP_PLUS:
            op = "+";
            break;
        case OP_MINUS:
            op = "-";
            break;
        case OP_MUL:
            op = "*";
            break;
        case OP_DIV:
            op = "/";
            break;
        case OP_MOD:
            op = "%";
            break;
        case OP_XOR:
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
            op = "&";
            break;
        case OP_BOR:
            op = "|";
            break;
        default:
            op = "unknown op";
            break;   
    }

    check_expr(comp, b->left);

    check_expr(comp, b->right);   

}

static void
check_unary(M1_compiler *comp, m1_unexpr *u) {
    char *op;
    m1_type t;
    
    switch (u->op) {
        case UNOP_POSTINC:

            op = "++";
            break;
        case UNOP_PREINC:

            op = "++";
            break;
        case UNOP_POSTDEC:

            op = "--";
            break;
        case UNOP_PREDEC:

            op = "--";
            break;
        default:
            op = "unknown op";
            break;   
    }    

    t = check_expr(comp, u->expr); 
    
    if (t != TYPE_INT) {
        type_error(comp, 0, "cannot apply '++' operator on non-integer type");   
    }
   

}

static void
check_break(M1_compiler *comp) {
    
}

static m1_type
check_funcall(M1_compiler *comp, m1_funcall *f) {
    m1_type rettype;
    
    return rettype;
}

static void
check_switch(M1_compiler *comp, m1_switch *s) {
    if (s->cases == NULL && s->defaultstat == NULL) {
        warning(comp, 0, "no cases nor a default statement in switch statement");   
    }   
}

static m1_type
check_expr(M1_compiler *comp, m1_expression *e) {
    m1_type t;
    if (e == NULL) 
        return t;
        
    switch (e->type) {
        case EXPR_NUMBER:
            check_number(comp, e->expr.l->value.fval);
            break;
        case EXPR_INT:
            check_int(comp, e->expr.l->value.ival);
            break;
        case EXPR_BINARY:
            check_binary(comp, e->expr.b);
            break;
        case EXPR_UNARY:
            check_unary(comp, e->expr.u);
            break;
        case EXPR_FUNCALL:
            check_funcall(comp, e->expr.f);
            break;
        case EXPR_ASSIGN:
            check_assign(comp, e->expr.a);
            break;
        case EXPR_IF:   
            check_if(comp, e->expr.i);
            break;
        case EXPR_WHILE:
            check_while(comp, e->expr.w);
            break;
        case EXPR_DOWHILE:
            check_dowhile(comp, e->expr.w);
            break;
        case EXPR_FOR:
            check_for(comp, e->expr.o);
            break;
        case EXPR_RETURN:
            check_return(comp, e->expr.e);
            break;
        case EXPR_NULL:
            check_null(comp);
            break;
        case EXPR_DEREF:
            check_deref(comp, e->expr.t);
            break;
        case EXPR_ADDRESS:
            check_address(comp, e->expr.t);
            break;
        case EXPR_OBJECT:
            check_obj(comp, e->expr.t);
            break;
        case EXPR_BREAK:
            check_break(comp);
            break;            
        case EXPR_CONSTDECL:
            break;
        case EXPR_VARDECL:
            break;
        case EXPR_SWITCH:
            check_switch(comp, e->expr.s);
            break;
        default:
            fprintf(stderr, "unknown expr type");   
            exit(EXIT_FAILURE);
    }   
    return t;
}

static void 
check_chunk(M1_compiler *comp, m1_chunk *c) {
    m1_expression *iter = c->block;
    
    
    while (iter != NULL) {
        (void)check_expr(comp, iter);
        iter = iter->next;
    }
}

void 
check(M1_compiler *comp, m1_chunk *ast) {
    m1_chunk *iter = ast;
    
    while (iter != NULL) {        
        check_chunk(comp, iter);
        iter = iter->next;   
    }
}

