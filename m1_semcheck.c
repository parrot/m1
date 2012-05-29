#include <stdio.h>
#include <stdlib.h>
#include "m1_symtab.h"
#include "m1_semcheck.h"
#include "m1_ast.h"

#include <assert.h>


static m1_type check_expr(M1_compiler *comp, m1_expression *e);
static void check_block(M1_compiler *comp, m1_expression *expr);

static void
type_error(M1_compiler *comp, unsigned line, char *msg) {
    fprintf(stderr, "Error (line %d): %s\n", line, msg);
    ++comp->errors;   
}

static void
warning(M1_compiler *comp, unsigned line, char *msg) {
    fprintf(stderr, "Warning (line %d): %s\n", line, msg);   
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
        type_error(comp, 0, "condition in while statement is not a boolean expression");       
    }

    check_block(comp, w->block);

}

static void
check_dowhile(M1_compiler *comp, m1_whileexpr *w) {
    m1_type condtype;

    condtype = check_expr(comp, w->cond);
    if (condtype != TYPE_BOOL) {
        type_error(comp, 0, "condition in do-while statement is not a boolean expression");   
    }
    
    check_block(comp, w->block);

}

static void
check_for(M1_compiler *comp, m1_forexpr *i) {

    if (i->init)
        check_expr(comp, i->init);

    if (i->cond) {
        m1_type t = check_expr(comp, i->cond);
        if (t != TYPE_BOOL) {
            type_error(comp, 0, "condition in for-loop is not a boolean expression");   
        }
        
    }

    if (i->step)
        check_expr(comp, i->step);

    if (i->block)
        check_block(comp, i->block);

}

static void
check_if(M1_compiler *comp, m1_ifexpr *i) {

    m1_type condtype = check_expr(comp, i->cond);
    if (condtype != TYPE_BOOL) {
        type_error(comp, 0, "condition in if-statement does not yield boolean value");   
    }

    check_block(comp, i->ifblock);

    if (i->elseblock) {
        check_block(comp, i->elseblock);
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
    m1_type funtype;
    m1_type rettype = check_expr(comp, e);
    if (funtype != rettype) {
        type_error(comp, 0, "type of return expression does not match function's return type");   
    }
    return funtype;
}

static m1_type
check_binary(M1_compiler *comp, m1_binexpr *b) {
    char *op;
    m1_type ltype, rtype;
    
    ltype = check_expr(comp, b->left);
    rtype = check_expr(comp, b->right);   


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
        case OP_OR:
            if (ltype != TYPE_BOOL) {
                type_error(comp, 0, "left-hand expression in boolean expression is not boolean");
            }
            if (rtype != TYPE_BOOL) {
                type_error(comp, 0, "right-hand expression in boolean expression is not boolean");    
            }
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

    
    return ltype;
}

static m1_type
check_unary(M1_compiler *comp, m1_unexpr *u) {
    m1_type t;

    t = check_expr(comp, u->expr);     
    switch (u->op) {
        case UNOP_POSTINC:
        case UNOP_PREINC:
        case UNOP_POSTDEC:
        case UNOP_PREDEC:   
            if (t != TYPE_INT) {
                type_error(comp, 0, "cannot apply '++' operator on non-integer expression");   
            }                    
            break;        
        case UNOP_NOT:
            if (t != TYPE_BOOL) {
                type_error(comp, 0, "cannot apply '!' operator on non-boolean expression");
            }
            break;
        default:
            break;   
    }    
    return t;

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
            return TYPE_NUM;

        case EXPR_INT:
            return TYPE_INT;

        case EXPR_STRING:
            return TYPE_STRING;

        case EXPR_BINARY:
            return check_binary(comp, e->expr.b);
            
        case EXPR_UNARY:
            return check_unary(comp, e->expr.u);

        case EXPR_FUNCALL:
            return check_funcall(comp, e->expr.f);

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
            return TYPE_NULL;
            break;
        case EXPR_DEREF:
            return check_deref(comp, e->expr.t);
            break;
        case EXPR_ADDRESS:
            return check_address(comp, e->expr.t);
            break;
        case EXPR_OBJECT:
            return check_obj(comp, e->expr.t);
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

static void
check_block(M1_compiler *comp, m1_expression *expr) {
    m1_expression *iter = expr;
    while (iter != NULL) {
        check_expr(comp, iter);
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

