#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

#include "symtab.h"
#include "semcheck.h"
#include "ast.h"
#include "decl.h"
#include "stack.h"




static m1_type check_expr(M1_compiler *comp, m1_expression *e);
static void check_block(M1_compiler *comp, m1_block *expr);
static m1_type check_obj(M1_compiler *comp, m1_object *obj, unsigned line);


static void
type_error(M1_compiler *comp, unsigned line, char *msg) {
    fprintf(stderr, "Error (line %d): %s.\n", line, msg);
    ++comp->errors;   
}

static void
type_error_extra(M1_compiler *comp, unsigned line, char *msg, ...) {
    va_list argp;
    char *p;
    char *s;
    int i;
    char fmtbuf[256];
        
    ++comp->errors;  
    fprintf(stderr, "Error (line %d): ", line);  
    va_start(argp, msg);
           
    for (p = msg; *p != '\0'; p++) {
        if (*p != '%') {
            putchar(*p);
            continue;
        }
        switch (*++p) {
            case 'd':
                i = va_arg(argp, int);
                sprintf(fmtbuf, "%d", i);
			    fputs(fmtbuf, stderr);
    			break;

            case 's': 
                s = va_arg(argp, char *);
                fputs(s, stderr);
                break;
            case '%':
                putchar('%');
                break;
            default:
                fprintf(stderr, "unrecognized error reporting format\n");
                assert(0);
                
        }       
               
    }
    
    va_end(argp);
}

static void
warning(M1_compiler *comp, unsigned line, char *msg) {
    assert(comp != NULL);
    fprintf(stderr, "Warning (line %d): %s\n", line, msg);   
    ++comp->warnings;
}


/*

Check assignments.

Compare the types of the target (lhs) and the expression
that is assigned to the target (rhs). They don't need to
be exactly the same, but they need to be compatible.



*/
static void
check_assign(M1_compiler *comp, m1_assignment *a, unsigned line) {
    m1_type ltype = check_obj(comp, a->lhs, line);
    m1_type rtype = check_expr(comp, a->rhs);
    
   
    if (ltype != rtype) {
        type_error(comp, line, 
                   "type of left expression does not match type "
                   "of right expression in assignment\n");   
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
check_obj(M1_compiler *comp, m1_object *obj, unsigned line) {
    m1_type t;
    if (obj->type == OBJECT_LINK) {
        
        t = check_obj(comp, obj->parent, line);   
        check_obj(comp, obj->obj.field, line);
        
    }
    
    switch (obj->type) {
        case OBJECT_MAIN: {
            /* look up identifier's declaration. */
            fprintf(stderr, "[semcheck] looking for %s...", obj->obj.name);
            m1_symbol *sym = sym_lookup_symbol(comp->currentsymtab, obj->obj.name);
            
            
            if (sym == NULL) {
                type_error_extra(comp, line, "Undeclared variable '%s'\n", obj->obj.name);
            }
            else
                fprintf(stderr, "found\n");
            obj->sym = sym;      
            break;
        }
        case OBJECT_FIELD:

            break;
        case OBJECT_DEREF:

            break;
        case OBJECT_INDEX: {
            m1_type t;
            t = check_expr(comp, obj->obj.index);
            if (t != TYPE_INT) {
                type_error(comp, line, "result of expression does not yield an integer value!\n");   
            }

            break;          
        }  
        default:
            break;
    }      
    return t;
    
}

static void
check_while(M1_compiler *comp, m1_whileexpr *w, unsigned line) {

    m1_type condtype = check_expr(comp, w->cond);
    push(comp->breakstack, 1);
    
    if (condtype != TYPE_BOOL) {
        warning(comp, line, "condition in while statement is not a boolean expression\n");       
    }

    check_expr(comp, w->block);
    (void)pop(comp->breakstack);
    
}

static void
check_dowhile(M1_compiler *comp, m1_whileexpr *w, unsigned line) {
    m1_type condtype;
    push(comp->breakstack, 1);
    
    condtype = check_expr(comp, w->cond);
    if (condtype != TYPE_BOOL) {
        warning(comp, line, "condition in do-while statement is not a boolean expression\n");   
    }
    
    check_expr(comp, w->block);
    
    (void)pop(comp->breakstack);
}

static void
check_for(M1_compiler *comp, m1_forexpr *i, unsigned line) {

    push(comp->breakstack, 1);
    
    if (i->init)
        check_expr(comp, i->init);

    if (i->cond) {
        m1_type t = check_expr(comp, i->cond);
        if (t != TYPE_BOOL) {
            warning(comp, line, "condition in for-loop is not a boolean expression\n");   
        }
        
    }

    if (i->step)
        check_expr(comp, i->step);

    if (i->block)
        check_expr(comp, i->block);

    (void)pop(comp->breakstack);
}

static void
check_if(M1_compiler *comp, m1_ifexpr *i, unsigned line) {

    m1_type condtype = check_expr(comp, i->cond);
    if (condtype != TYPE_BOOL) {
        warning(comp, line, "condition in if-statement does not yield boolean value\n");   
    }

    check_expr(comp, i->ifblock);

    if (i->elseblock) {
        check_expr(comp, i->elseblock);
    }
           
}

static m1_type
check_deref(M1_compiler *comp, m1_object *o, unsigned line) {
    m1_type t = check_obj(comp, o, line);
    return t;
}

static m1_type
check_address(M1_compiler *comp, m1_object *o, unsigned line) {
    m1_type t = check_obj(comp, o, line);   
    return t;
}

static m1_type
check_return(M1_compiler *comp, m1_expression *e, unsigned line) {
    m1_type funtype;
    m1_type rettype = check_expr(comp, e);
    if (funtype != rettype) {
        type_error(comp, line, "type of return expression does not match function's return type");   
    }
    return funtype;
}

static m1_type
check_binary(M1_compiler *comp, m1_binexpr *b, unsigned line) {
    m1_type ltype, rtype;
    
    ltype = check_expr(comp, b->left);
    rtype = check_expr(comp, b->right);   

    if (ltype != rtype) {
        type_error(comp, line, "types of left and right operands do not match");   
    }
    
    switch(b->op) {
        case OP_PLUS:
        case OP_MINUS:
        case OP_MUL:
        case OP_DIV:       
        case OP_MOD:
        case OP_XOR:
            if (ltype != TYPE_INT || ltype != TYPE_NUM || rtype != TYPE_INT || rtype != TYPE_NUM) {
                type_error(comp, line, "mathematical operator needs integer or floating-point expressions as operands");   
            }
            break;
        case OP_GT:
        case OP_GE:
        case OP_LT:
        case OP_LE:
        case OP_EQ:
        case OP_NE:
            if (ltype == TYPE_NUM) {
                warning(comp, line, "comparing floating-point numbers may not yield correct results");       
            }
            else if (ltype == TYPE_STRING) {
                type_error(comp, line, "cannot apply comparison operator on strings");   
            }
            break;
        case OP_AND:
        case OP_OR:
            if (ltype != TYPE_BOOL) {
                type_error(comp, line, "left-hand expression in boolean expression is not boolean");
            }
            if (rtype != TYPE_BOOL) {
                type_error(comp, line, "right-hand expression in boolean expression is not boolean");    
            }
            break;
        case OP_BAND:
        case OP_BOR:
            if (ltype != TYPE_INT) {
                type_error(comp, line, "cannot apply binary & or | operator on non-integer expressions");   
            }
            break;
        default:
            break;   
    }

    
    return ltype;
}

static m1_type
check_unary(M1_compiler *comp, m1_unexpr *u, unsigned line) {
    m1_type t = TYPE_VOID;

    t = check_expr(comp, u->expr);     
    switch (u->op) {
        case UNOP_POSTINC:
        case UNOP_PREINC:
            if (t != TYPE_INT) {
                type_error(comp, line, "cannot apply '++' operator on non-integer expression");   
            }                    
            break;        
        case UNOP_POSTDEC:
        case UNOP_PREDEC:   
            if (t != TYPE_INT) {
                type_error(comp, line, "cannot apply '--' operator on non-integer expression");   
            }                    
            break;        
        case UNOP_NOT:
            if (t != TYPE_BOOL) {
                type_error(comp, line, "cannot apply '!' operator on non-boolean expression");
            }
            break;
        default:
            break;   
    }    
    return t;

}

static void
check_break(M1_compiler *comp, unsigned line) {
    if (top(comp->breakstack) == 0) {
        type_error(comp, line, "Cannot use 'break' in non-iterating block.");
    }

    assert(comp != NULL);
}

static void
check_continue(M1_compiler *comp, unsigned line) {
    if (top(comp->breakstack) == 0) {
        type_error(comp, line, "Cannot use 'continue' in non-iterating block.");
    }
    assert(comp != NULL);
}

static m1_type
check_funcall(M1_compiler *comp, m1_funcall *f, unsigned line) {
    m1_type rettype;
    
    assert(comp != NULL);
    assert(f != NULL);    
    assert(line != 0);
    
    /* find declaration of function, check arguments against function signature. */
    /* TODO */
    return rettype;
}

static void
check_switch(M1_compiler *comp, m1_switch *s, unsigned line) {
    if (s->cases == NULL && s->defaultstat == NULL) {
        warning(comp, line, "no cases nor a default statement in switch statement");   
    }   
}

static void
check_newexpr(M1_compiler *comp, m1_newexpr *n, unsigned line) {
    assert(comp != NULL);
    assert(n != NULL);

    n->typedecl = type_find_def(comp, n->type); /* find the decl for requested type */
    
    if (n->typedecl == NULL) { 
        type_error_extra(comp, line, "Cannot find type '%s' requested for in new-statement\n", n->type);         
    }
    
}

static void
check_vardecl(M1_compiler *comp, m1_var *v, unsigned line) {
    if (v->init) 
        check_expr(comp, v->init);   
    
    assert(v->sym != NULL);
    
    /* find the type declaration for the specified type. */
    v->sym->typedecl = type_find_def(comp, v->type);

    if (v->sym->typedecl == NULL) {        
        type_error_extra(comp, line, "Cannot find type '%s'\n", v->type);   
    }
        
    if (v->next) {
        check_vardecl(comp, v->next, line);   
    }
}

static void
check_cast(M1_compiler *comp, m1_castexpr *expr, unsigned line) {
    if (strcmp(expr->type, "int") == 0) {
        expr->targettype = VAL_INT;
    }
    else if (strcmp(expr->type, "num") == 0) {
        expr->targettype = VAL_FLOAT;
    }
    else {
        type_error_extra(comp, line, "Cannot cast value to type %s", expr->type);
    }    
}

static m1_type
check_expr(M1_compiler *comp, m1_expression *e) {
    m1_type t = TYPE_VOID;
    
    if (e == NULL) 
        return t;
        
    switch (e->type) {
        case EXPR_BLOCK:
            check_block(comp, e->expr.blck);
            break;
        case EXPR_CAST:
            check_cast(comp, e->expr.cast, e->line);
            break;
        case EXPR_NUMBER:
            return TYPE_NUM;

        case EXPR_INT:
            return TYPE_INT;

        case EXPR_STRING:
            return TYPE_STRING;
        case EXPR_TRUE:
        case EXPR_FALSE:
            return TYPE_BOOL;
            
        case EXPR_BINARY:
            return check_binary(comp, e->expr.b, e->line);
            
        case EXPR_UNARY:
            return check_unary(comp, e->expr.u, e->line);

        case EXPR_FUNCALL:
            return check_funcall(comp, e->expr.f, e->line);

        case EXPR_ASSIGN:
            check_assign(comp, e->expr.a, e->line);
            break;
        case EXPR_IF:   
            check_if(comp, e->expr.i, e->line);
            break;
        case EXPR_WHILE:
            check_while(comp, e->expr.w, e->line);
            break;
        case EXPR_DOWHILE:
            check_dowhile(comp, e->expr.w, e->line);
            break;
        case EXPR_FOR:
            check_for(comp, e->expr.o, e->line);
            break;
        case EXPR_RETURN:
            return check_return(comp, e->expr.e, e->line);
            break;
        case EXPR_NULL:
            return TYPE_NULL;
            break;
        case EXPR_DEREF:
            return check_deref(comp, e->expr.t, e->line);
            break;
        case EXPR_ADDRESS:
            return check_address(comp, e->expr.t, e->line);
            break;
        case EXPR_OBJECT:
            return check_obj(comp, e->expr.t, e->line);
            break;
        case EXPR_BREAK:
            check_break(comp, e->line);
            break;            
        case EXPR_CONTINUE:
            check_continue(comp, e->line);
            break;   
        case EXPR_CONSTDECL:
            break;
        case EXPR_VARDECL:
            check_vardecl(comp, e->expr.v, e->line);
            break;
        case EXPR_SWITCH:
            check_switch(comp, e->expr.s, e->line);
            break;
            
        case EXPR_NEW:
            check_newexpr(comp, e->expr.n, e->line);
            break;
        case EXPR_PRINT:
            check_expr(comp, e->expr.e);
            break;
                
        default:
            fprintf(stderr, "unknown expr type (%d)", e->type);   
            assert(0); /* shouldn't happen. */
    }   
    return t;
}




static void
check_block(M1_compiler *comp, m1_block *block) {
    assert(block != NULL);
    m1_expression *iter = block->stats;
    
    assert(&block->locals != NULL);
    
    /* set currentsymtab to this block's symbol table. */
    comp->currentsymtab = &block->locals;
    
    while (iter != NULL) {
        check_expr(comp, iter);
        iter = iter->next;   
    }   
    /* closing block, so set current symbol table to the parent scope's symtab. */
    comp->currentsymtab = block->locals.parentscope;
}

static void 
check_chunk(M1_compiler *comp, m1_chunk *c) {
    /* We're in a block, but not a block to break out of. 
       Ensure that top() won't fail, if there's nothing on the stack.
       The value 0 is to reflect that no break or continue is allowed.
       When checking while, do-while and for, a 1 is pushed. This needs
       to be done on a stack, since iterating statements may be nested.
     */
    push(comp->breakstack, 0); 
    
    check_block(comp, c->block);
    
    (void)pop(comp->breakstack); 
}


void 
check(M1_compiler *comp, m1_chunk *ast) {
    m1_chunk *iter = ast;
    
    while (iter != NULL) {       
        comp->currentchunk = iter;    
        check_chunk(comp, iter);
        iter = iter->next;   
    }
}

