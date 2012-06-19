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




static m1_decl *check_expr(M1_compiler *comp, m1_expression *e);

static void check_block(M1_compiler *comp, m1_block *expr);

static m1_decl *check_obj(M1_compiler *comp, m1_object *obj, unsigned line, m1_object **parent);

/* Cache these built-in types. Read-only. */
static m1_decl *BOOLTYPE;
static m1_decl *INTTYPE;
static m1_decl *NUMTYPE;
static m1_decl *STRINGTYPE;
static m1_decl *VOIDTYPE;

/* XXX is this init routine thread-safe? */
static void
init_typechecker(M1_compiler *comp) {
    BOOLTYPE   = type_find_def(comp, "bool"); 
    INTTYPE    = type_find_def(comp, "int");
    NUMTYPE    = type_find_def(comp, "num");
    STRINGTYPE = type_find_def(comp, "string");  
    VOIDTYPE   = type_find_def(comp, "void");
}


static void
type_error(M1_compiler *comp, unsigned line, char *msg, ...) {
    va_list argp;
    char *p;
    char *s;
    int i;
    char fmtbuf[256];
        
    ++comp->errors;  
    fprintf(stderr, "%s:%d: error: ", comp->current_filename, line);  
    
    va_start(argp, msg);
           
    for (p = msg; *p != '\0'; p++) {
        if (*p != '%') {
            putc(*p, stderr);

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
    fprintf(stderr, "\n");
    
    va_end(argp);
}

static void
warning(M1_compiler *comp, unsigned line, char *msg) {
    assert(comp != NULL);
    fprintf(stderr, "%s:%d: warning: %s\n", comp->current_filename, line, msg);   
    ++comp->warnings;
}



/*

Check assignments.

Compare the types of the target (lhs) and the expression
that is assigned to the target (rhs). They don't need to
be exactly the same, but they need to be compatible.

*/
static m1_decl *
check_assign(M1_compiler *comp, m1_assignment *a, unsigned line) {
    m1_object *parent; /* storage on the C runtime stack for check_obj. */
    m1_decl *ltype = check_obj(comp, a->lhs, line, &parent);
    m1_decl *rtype = check_expr(comp, a->rhs);
    
    assert(ltype != NULL);
    assert(rtype != NULL);
    
    /* pointer comparison is fine, since each type is only stored once in 
       the type table. 
     */
    if (ltype != rtype) { 
        type_error(comp, line, "type of left expression (%s) does not match type "
                               "of right expression (%s) in assignment", 
                               ltype->name, rtype->name);   
    }
    return rtype;
}


static m1_decl *
check_obj(M1_compiler *comp, m1_object *obj, unsigned line, m1_object **parent) 
{
    m1_decl *t = VOIDTYPE;

    switch (obj->type) 
    {        
        case OBJECT_LINK: {
            m1_decl *fieldtype;
            
            *parent = obj;
            
            t = check_obj(comp, obj->parent, line, parent);   
            /* in case field is a member of a struct, get _that_ type;
               when it's an index, return the type of obj->parent. 
             */
            fieldtype = check_obj(comp, obj->obj.field, line, parent); 
            if (obj->obj.field->type == OBJECT_FIELD) 
                t = fieldtype;
            
            break;   
        }
        case OBJECT_MAIN: {

            /* look up identifier's declaration. */
            obj->sym = sym_lookup_symbol(comp->currentsymtab, obj->obj.name);            

            if (obj->sym == NULL) {
                type_error(comp, line, "Undeclared variable '%s'", obj->obj.name);
            }
            else { /* found symbol, now link it to the object node. */
                /* find the type definition for this symbol's type. */
                obj->sym->typedecl = type_find_def(comp, obj->sym->type_name);
                if (obj->sym->typedecl == NULL) {
                    type_error(comp, line, "Type '%s' is not defined", obj->sym->type_name);   
                }
                t = obj->sym->typedecl;

                assert(t != NULL);
            }             
            *parent = obj;              
            break;
        }
        case OBJECT_FIELD: {

            assert((*parent)->sym != NULL);
            assert((*parent)->sym->typedecl != NULL);

            obj->sym = sym_lookup_symbol( &(*parent)->sym->typedecl->d.s->sfields, obj->obj.name);

            if (obj->sym == NULL) {
                type_error(comp, line, "Struct %s has no member %s", 
                           (*parent)->obj.name, obj->obj.name);
            }
            else {
                /* find the type declaration for this field's type. */
                obj->sym->typedecl = type_find_def(comp, obj->sym->type_name);
                if (obj->sym->typedecl == NULL) {
                    type_error(comp, line, "Type '%s' is not defined", obj->sym->type_name);   
                }
                t = obj->sym->typedecl;    
            }
            *parent = obj;
            break;
        }
        case OBJECT_DEREF:
            fprintf(stderr, "Error (line %d): a->b is not implemented!\n", line);
            assert(0);
            break;
        case OBJECT_INDEX: 
            t = check_expr(comp, obj->obj.index);
            if (t != INTTYPE) {
                type_error(comp, line, "result of expression does not yield an integer value");   
            }
            break;                    
        default:
            break;
    }      
    return t;
    
}

static void
check_while(M1_compiler *comp, m1_whileexpr *w, unsigned line) {    
    m1_decl *condtype = check_expr(comp, w->cond);
        
    if (condtype != BOOLTYPE) {
        warning(comp, line, "condition in while statement is not a boolean expression");       
    }

    push(comp->breakstack, 1);
    push(comp->continuestack, 1);       
    
    (void)check_expr(comp, w->block);
    
    (void)pop(comp->breakstack);
    (void)pop(comp->continuestack);    
}

static void
check_dowhile(M1_compiler *comp, m1_whileexpr *w, unsigned line) {
    m1_decl *condtype;
    
    
    condtype = check_expr(comp, w->cond);
 
    if (condtype != BOOLTYPE) {
        warning(comp, line, "condition in do-while statement is not a boolean expression");   
    }

    push(comp->breakstack, 1);
    push(comp->continuestack, 1);    
    
    (void)check_expr(comp, w->block);  
      
    (void)pop(comp->breakstack);
    (void)pop(comp->continuestack);    
}

static void
check_for(M1_compiler *comp, m1_forexpr *i, unsigned line) {
    /* break and continue are allowed in for loops. */
    
    if (i->init)
        check_expr(comp, i->init);

    if (i->cond) {
        m1_decl *t = check_expr(comp, i->cond);
        if (t != BOOLTYPE) {
            warning(comp, line, "condition in for-loop is not a boolean expression");   
        }        
    }

    if (i->step)
        (void)check_expr(comp, i->step);

    push(comp->breakstack, 1); 
    push(comp->continuestack, 1);

    if (i->block)
        (void)check_expr(comp, i->block);

    (void)pop(comp->breakstack);
    (void)pop(comp->continuestack);
}

static void
check_if(M1_compiler *comp, m1_ifexpr *i, unsigned line) {

    m1_decl *condtype = check_expr(comp, i->cond);
    if (condtype != BOOLTYPE) {
        warning(comp, line, "condition in if-statement does not yield boolean value");   
    }

    (void)check_expr(comp, i->ifblock);

    if (i->elseblock) {
        (void)check_expr(comp, i->elseblock);
    }
           
}

static m1_decl *
check_deref(M1_compiler *comp, m1_object *o, unsigned line) {
    /* declared here to use the storage space on C runtime stack. */
    m1_object *parent; 
    m1_decl *t = check_obj(comp, o, line, &parent);
    /* XXX *obj not implemented yet. */
    return t;
}

static m1_decl *
check_address(M1_compiler *comp, m1_object *o, unsigned line) {
    /* declared here to use the storage space on C runtime stack. */
    m1_object *parent; 
    m1_decl *t = check_obj(comp, o, line, &parent);   
    /* XXX &obj not implemented yet. */
    return t;
}

static m1_decl *
check_return(M1_compiler *comp, m1_expression *e, unsigned line) {
    /* find type of current chunk */
    m1_decl *funtype = type_find_def(comp, comp->currentchunk->rettype);    
    m1_decl *rettype = VOIDTYPE;
    
    if (e != NULL) {
        rettype = check_expr(comp, e);
    }
    
    if (funtype != rettype) {
        type_error(comp, line, "type of return expression does not match function's return type");   
    }
    return rettype; /* return type of the expression */
}

static m1_decl *
check_binary(M1_compiler *comp, m1_binexpr *b, unsigned line) {
    m1_decl *ltype, 
            *rtype;
    
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
            if (ltype != INTTYPE && ltype != NUMTYPE 
                && rtype != INTTYPE && rtype != NUMTYPE) 
            {
                type_error(comp, line, "mathematical operator needs integer or "
                                       "floating-point expressions as operands");   
            }
            break;
        case OP_GT:
        case OP_GE:
        case OP_LT:
        case OP_LE:
        case OP_EQ:
        case OP_NE:
            if (ltype == NUMTYPE) {
                warning(comp, line, 
                        "comparing floating-point numbers may not yield correct results");       
            }
            else if (ltype == STRINGTYPE) {
                type_error(comp, line, "cannot apply comparison operator on strings");   
            }
            break;
        case OP_AND:
        case OP_OR:
            if (ltype != BOOLTYPE) {
                warning(comp, line, "left-hand expression in boolean expression is not boolean");
            }
            if (rtype != BOOLTYPE) {
                warning(comp, line, "right-hand expression in boolean expression is not boolean");    
            }
            break;
        case OP_BAND:
        case OP_BOR:
            if (ltype != INTTYPE) {
                type_error(comp, line, 
                           "cannot apply binary & or | operator on non-integer expressions");   
            }
            break;
        default:
            break;   
    }

    
    return ltype;
}

static m1_decl *
check_unary(M1_compiler *comp, m1_unexpr *u, unsigned line) {
    m1_decl *t = VOIDTYPE;

    t = check_expr(comp, u->expr);     
    switch (u->op) {
        case UNOP_POSTINC:
        case UNOP_PREINC:
            if (t != INTTYPE) {
                type_error(comp, line, "cannot apply '++' operator on non-integer expression");   
            }                    
            break;        
        case UNOP_POSTDEC:
        case UNOP_PREDEC:   
            if (t != INTTYPE) {
                type_error(comp, line, "cannot apply '--' operator on non-integer expression");   
            }                    
            break;        
        case UNOP_NOT:
            if (t != BOOLTYPE) {
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
        type_error(comp, line, "Cannot use break in non-iterating block");
    }
}

static void
check_continue(M1_compiler *comp, unsigned line) {
    if (top(comp->continuestack) == 0) {
        type_error(comp, line, "cannot use continue in non-iterating block");
    }
}

static m1_decl *
check_funcall(M1_compiler *comp, m1_funcall *f, unsigned line) {
    
    assert(comp != NULL);
    assert(f != NULL);    
    assert(line != 0);
        
    f->funsym = sym_lookup_symbol(comp->globalsymtab, f->name);
    //f->funsym = sym_find_chunk(comp->globalsymtab, f->name);
    
    if (f->funsym == NULL) {
        type_error(comp, line, "function '%s' not defined", f->name);
        return NULL;    
    }
    /* set the function's return type declaration as stored in the symbol. */
    if (f->funsym->typedecl == NULL) { 
        /* type wasn't declared yet at time that function was defined. */
        f->funsym->typedecl = f->typedecl = type_find_def(comp, f->funsym->type_name);
        
        if (f->typedecl == NULL) {
            type_error(comp, line, "return type '%s' of function '%s' is not defined", 
                       f->funsym->type_name, f->name);               
        }   
        
    }
    else {
        assert(f->funsym->typedecl != NULL);
        f->typedecl = f->funsym->typedecl;
    }
    
    /* check arguments of the function call; this is a list of expressions. */
    if (f->arguments != NULL)
        (void)check_expr(comp, f->arguments);
    

    /* XXX find declaration of function, check arguments against 
    function signature. */
    /* TODO */
    
    return f->typedecl;
}

static void
check_exprlist(M1_compiler *comp, m1_expression *expr) {
    m1_expression *iter = expr;
    while (iter != NULL) {
        check_expr(comp, iter);
        iter = iter->next;   
    }   
}

static void
check_switch(M1_compiler *comp, m1_switch *s, unsigned line) {
    push(comp->breakstack, 1);
    
    if (s->cases == NULL && s->defaultstat == NULL) {
        warning(comp, line, "no cases nor a default statement in switch statement");   
    }   
    (void)check_expr(comp, s->selector);
    
    if (s->cases) {
        m1_case *iter = s->cases;
        while (iter != NULL) {
            (void)check_exprlist(comp, iter->block);
            iter = iter->next;   
        }   
    }
    if (s->defaultstat)
        (void)check_expr(comp, s->defaultstat);
        
    (void)pop(comp->breakstack);
}

static m1_decl *
check_newexpr(M1_compiler *comp, m1_newexpr *n, unsigned line) {
    assert(comp != NULL);
    assert(n != NULL);

    /* find the decl for requested type */
    n->typedecl = type_find_def(comp, n->type); 
    
    if (n->typedecl == NULL) { 
        type_error(comp, line, "Cannot find type '%s' requested for in new-statement", n->type);         
    }
    return n->typedecl;
}

static void
check_vardecl(M1_compiler *comp, m1_var *v, unsigned line) {        
    assert(v->sym != NULL);
    
    /* find the type declaration for the specified type. */
    v->sym->typedecl = type_find_def(comp, v->type);

    if (v->sym->typedecl == NULL) {        
        type_error(comp, line, 
                   "Cannot find type '%s' for variable '%s'", v->type, v->name);   
    }
    else {
        /* now check the type of the initialization expression and check 
           compatibility with type of variable. Only do this check if 
           v->sym->typedecl was found.
        */
        if (v->init) {
            m1_decl *inittype = check_expr(comp, v->init);   
               
            if (inittype != v->sym->typedecl) {
                type_error(comp, line, 
                           "Incompatible types in initialization of variable %s.", v->name);       
            }            
        }
    }
        
    if (v->next) {
        (void)check_vardecl(comp, v->next, line);   
    }
}

static m1_decl *
check_cast(M1_compiler *comp, m1_castexpr *expr, unsigned line) {
    m1_decl *type = check_expr(comp, expr->expr);
    if (strcmp(expr->type, "int") == 0) {
        expr->targettype = VAL_INT;
        type = INTTYPE;
    }
    else if (strcmp(expr->type, "num") == 0) {
        expr->targettype = VAL_FLOAT;
        type = NUMTYPE;
    }
    else {
        type_error(comp, line, "Cannot cast value to type %s", expr->type);
        type = type_find_def(comp, expr->type);
    }   
    return type; 
}

static void
check_print_arg(M1_compiler *comp, m1_expression *e) {
    if (e == NULL)
        return;
        
    /* go to end of list recursively; list of arguments is in reversed order. */     
    check_print_arg(comp, e->next);
    
    (void)check_expr(comp, e);       
}

static m1_decl *
check_expr(M1_compiler *comp, m1_expression *e) {
    m1_decl *t = VOIDTYPE;
       
    assert (e != NULL);
                
    switch (e->type) {
        case EXPR_ADDRESS:
            return check_address(comp, e->expr.t, e->line);        
        case EXPR_ASSIGN:
            return check_assign(comp, e->expr.a, e->line);       
        case EXPR_BINARY:
            return check_binary(comp, e->expr.b, e->line);
        case EXPR_BLOCK:
            check_block(comp, e->expr.blck);
            break;            
        case EXPR_BREAK:
            check_break(comp, e->line);
            break;                        
        case EXPR_CONTINUE:
            check_continue(comp, e->line);
            break;               
        case EXPR_CONSTDECL:
            break;            
        case EXPR_CAST:
            return check_cast(comp, e->expr.cast, e->line);
        case EXPR_CHAR:
            return INTTYPE;
        case EXPR_DEREF:
            return check_deref(comp, e->expr.t, e->line);        
        case EXPR_DOWHILE:
            check_dowhile(comp, e->expr.w, e->line);
            break;
        case EXPR_FALSE:
            return BOOLTYPE;            
        case EXPR_FOR:
            check_for(comp, e->expr.o, e->line);
            break;
        case EXPR_FUNCALL:
            return check_funcall(comp, e->expr.f, e->line);
        case EXPR_IF:   
            check_if(comp, e->expr.i, e->line);
            break;        
        case EXPR_INT:
            return INTTYPE;            
        case EXPR_NUMBER:
            return NUMTYPE;
        case EXPR_NEW:
            return check_newexpr(comp, e->expr.n, e->line);
        case EXPR_NULL:   
            break;
        case EXPR_OBJECT: {
            m1_object *parent; /* Provides storage on C runtime stack to use by check_obj.*/
            return check_obj(comp, e->expr.t, e->line, &parent);
        }
        case EXPR_PRINT:
            check_print_arg(comp, e->expr.e);
            break;            
        case EXPR_RETURN:
            return check_return(comp, e->expr.e, e->line);
        case EXPR_STRING:
            return STRINGTYPE;                        
        case EXPR_SWITCH:
            check_switch(comp, e->expr.s, e->line);
            break;
        case EXPR_TRUE:
            return BOOLTYPE;                                    
        case EXPR_UNARY:
            return check_unary(comp, e->expr.u, e->line);            
        case EXPR_VARDECL:
            check_vardecl(comp, e->expr.v, e->line);
            break;
        case EXPR_WHILE:
            check_while(comp, e->expr.w, e->line);
            break;                                       
        default:
            fprintf(stderr, "unknown expr type (%d)", e->type);   
            assert(0); /* shouldn't happen. */
    }   
    return t;
}




static void
check_block(M1_compiler *comp, m1_block *block) {
    m1_expression *iter;
    
    assert(block != NULL);    
    assert(&block->locals != NULL);    
        
    /* set currentsymtab to this block's symbol table. */
    comp->currentsymtab = &block->locals;

    iter = block->stats;
    while (iter != NULL) {
        (void)check_expr(comp, iter);
        iter = iter->next;   
    }
       
    /* closing block, so set current symbol table to parent scope's symtab. */
    comp->currentsymtab = block->locals.parentscope;
}

static void
check_parameters(M1_compiler *comp, m1_var *parameters, unsigned line) {
    m1_var *paramiter = parameters;
    while (paramiter != NULL) {
        (void)check_vardecl(comp, paramiter, line);
        
        assert(paramiter->sym != NULL);
        assert(paramiter->sym->typedecl != NULL);
        
        paramiter = paramiter->next;
    }   
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
    push(comp->continuestack, 0); /* not allowed in switch-statements. */
    
    check_parameters(comp, c->parameters, c->line);
    check_block(comp, c->block);
    
    (void)pop(comp->breakstack); 
    (void)pop(comp->continuestack);
}

/* go through members and assign an offset to them. */
static void
check_struct_decl(M1_compiler *comp, m1_struct *str) {
    /* members of a struct are stored in a symbol table. */
    m1_symbol *iter = sym_get_table_iter(&str->sfields);
    
    
    unsigned offset          = 0;
    unsigned size_of_current = 0;

    while (iter != NULL) 
    {        
        iter->offset = offset;
        
        if (iter->typedecl == NULL) {

            iter->typedecl = type_find_def(comp, iter->type_name);
            if (iter->typedecl == NULL) {
                type_error(comp, str->line_defined,
                           "Cannot find type '%s' for struct member '%s'", 
                           iter->type_name, iter->name);                      
            }
            else {
                fprintf(stderr, "check struct decl: type of member %s is %s and size is %d\n", iter->name, iter->typedecl->name, type_get_size(iter->typedecl));   
            }
        }
        
        /* add current field's size to offset, which will be next field's offset. */
        size_of_current = type_get_size(iter->typedecl) * iter->var->num_elems;
        offset += size_of_current; 
        
        iter = sym_iter_next(iter);
    }
    
    str->size = offset;
            
}

static void
check_pmc_decl(M1_compiler *comp, m1_pmc *pmc) {
    m1_symbol *iter = sym_get_table_iter(&pmc->sfields);
    
    
    unsigned offset          = 0;
    unsigned size_of_current = 0;

    fprintf(stderr, "[semcheck] checking PMC %s\n", pmc->name);
    
    while (iter != NULL) 
    {        
        iter->offset = offset;
        
        if (iter->typedecl == NULL) {

            iter->typedecl = type_find_def(comp, iter->type_name);
            if (iter->typedecl == NULL) {
                type_error(comp, pmc->line_defined,
                           "Cannot find type '%s' for PMC member '%s'", 
                           iter->type_name, iter->name);                      
            }
        }
        
        /* add current field's size to offset, which will be next field's offset. */
        size_of_current = type_get_size(iter->typedecl);
        offset += size_of_current; 
        
        iter = sym_iter_next(iter);
    }
    
    pmc->size = offset;
    fprintf(stderr, "[semcheck] Size of PMC %s is %d\n", pmc->name, offset);

}

/* Go through type declarations and do a sanity check. */
static void
check_decls(M1_compiler *comp) {
    m1_decl *iter = comp->declarations;
    
    while (iter != NULL) {
        switch (iter->decltype) {
            case DECL_STRUCT:
                check_struct_decl(comp, iter->d.s);
                break;
            case DECL_PMC:
                check_pmc_decl(comp, iter->d.p);
                break;
            default: /* ignore all other types. */
                break;    
        }
        iter = iter->next;   
    }
}


void 
check(M1_compiler *comp, m1_chunk *ast) {
    m1_chunk *iter = ast;
    
    init_typechecker(comp);
    
    /* check declarations of types first. */
    check_decls(comp);
    
    while (iter != NULL) {       
        comp->currentchunk = iter;    
        check_chunk(comp, iter);
        iter = iter->next;   
    }
}

