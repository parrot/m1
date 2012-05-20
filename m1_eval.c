/*

Sample AST walking code to print out the equivalent M1 code.

*/

#include "m1_eval.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define OUT stderr

static void eval_expr(m1_expression *e);

static void
eval_number(double value) {
    fprintf(OUT, "%f", value);
}   

static void
eval_int(int value) {
    fprintf(OUT, "%d", value);
}


static void
eval_assign(m1_assignment *a) {
    eval_expr(a->lhs);
    fprintf(OUT, " = ");
    eval_expr(a->rhs);
    fprintf(OUT, ";");
}

static void
eval_null(void) {
    fprintf(OUT, "null");    
}   

static void
eval_obj(m1_object *obj) {
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
            eval_expr(obj->obj.index);
            fprintf(OUT, "]");
            break;            
        default:
            break;
    }      
    
    if (obj->next) {
        eval_obj(obj->next);   
    }
}

static void
eval_while(m1_whileexpr *w) {
    fprintf(OUT, "while (");
    eval_expr(w->cond);
    fprintf(OUT, ") {\n");
    eval_expr(w->cond);
    fprintf(OUT, "}\n");   
}

static void
eval_dowhile(m1_whileexpr *w) {
    fprintf(OUT, "do {\n");
    eval_expr(w->block);
    fprintf(OUT, "}\n");       
    fprintf(OUT, "while (");
    eval_expr(w->cond);
    fprintf(OUT, ");\n");
}

static void
eval_for(m1_forexpr *i) {
    fprintf(OUT, "for (");
    if (i->init)
        eval_expr(i->init);

    if (i->cond)
        eval_expr(i->cond);
    fprintf(OUT, ";");
    if (i->step)
        eval_expr(i->step);
    fprintf(OUT, ") {\n");
    if (i->block)
        eval_expr(i->block);
    fprintf(OUT, "}");
}

static void
eval_if(m1_ifexpr *i) {
    fprintf(OUT, "if (");
    eval_expr(i->cond);
    fprintf(OUT, ") {\n");
    eval_expr(i->ifblock);
    fprintf(OUT, "}\n");
    if (i->elseblock) {
        fprintf(OUT, "else {\n");
        eval_expr(i->elseblock);
        fprintf(OUT, "}");
    }
           
}

static void
eval_deref(m1_object *o) {
    fprintf(OUT, "*");
    eval_obj(o);
}

static void
eval_address(m1_object *o) {
    fprintf(OUT, "&");
    eval_obj(o);   
}

static void
eval_return(m1_expression *e) {
    fprintf(OUT, "return ");
    eval_expr(e);
    fprintf(OUT, ";");    
}

static void
eval_binary(m1_binexpr *b) {
    char *op;
    eval_expr(b->left);
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
            op = "&";
            break;
        case OP_BOR:
            op = "|";
            break;
        default:
            op = "unknown op";
            break;   
    }
    fprintf(OUT, "%s", op);
    eval_expr(b->right);    
}

static void
eval_unary(m1_unexpr *u) {
    char *op;
    int postfix = 0;
    
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
    if (postfix != 0) {
        eval_expr(u->expr); 
        fprintf(OUT, "%s", op);
    }
    else {

        fprintf(OUT, "%s", op);
        eval_expr(u->expr);             
    }
    
    fprintf(OUT, ";");
}

static void
eval_break(void) {
    fprintf(OUT, "break;");   
}

static void
eval_funcall(m1_funcall *f) {
    fprintf(OUT, "%s();", f->name);   
}

static void
eval_expr(m1_expression *e) {
    if (e == NULL) 
        return;
        
    switch (e->type) {
        case EXPR_NUMBER:
            eval_number(e->expr.floatval);
            break;
        case EXPR_INT:
            eval_int(e->expr.intval);
            break;
        case EXPR_BINARY:
            eval_binary(e->expr.b);
            break;
        case EXPR_UNARY:
            eval_unary(e->expr.u);
            break;
        case EXPR_FUNCALL:
            eval_funcall(e->expr.f);
            break;
        case EXPR_ASSIGN:
            eval_assign(e->expr.a);
            break;
        case EXPR_IF:   
            eval_if(e->expr.i);
            break;
        case EXPR_WHILE:
            eval_while(e->expr.w);
            break;
        case EXPR_DOWHILE:
            eval_dowhile(e->expr.w);
            break;
        case EXPR_FOR:
            eval_for(e->expr.o);
            break;
        case EXPR_RETURN:
            eval_return(e->expr.e);
            break;
        case EXPR_NULL:
            eval_null();
            break;
        case EXPR_DEREF:
            eval_deref(e->expr.t);
            break;
        case EXPR_ADDRESS:
            eval_address(e->expr.t);
            break;
        case EXPR_OBJECT:
            eval_obj(e->expr.t);
            break;
        case EXPR_BREAK:
            eval_break();
            break;
        default:
            fprintf(stderr, "unknown expr type");   
            exit(EXIT_FAILURE);
    }   

}

static void 
eval_chunk(m1_chunk *c) {
    m1_expression *iter = c->block;
    fprintf(OUT, ".chunk '%s'\n", c->name);
    while (iter != NULL) {
        eval_expr(iter);
        iter = iter->next;
        fprintf(OUT, "\n");
    }
}

void 
eval(m1_chunk *ast) {
    m1_chunk *iter = ast;
    
    while (iter != NULL) {        
        eval_chunk(iter);
        iter = iter->next;   
    }
}

/*

code generation skeleton.

*/



static void gencode_expr(m1_expression *e);

static void
gencode_number(double value) {
    fprintf(OUT, "%f", value);
}   

static void
gencode_int(int value) {
    fprintf(OUT, "%d", value);
}


static void
gencode_assign(m1_assignment *a) {
    gencode_expr(a->lhs);
    fprintf(OUT, " = ");
    gencode_expr(a->rhs);
    fprintf(OUT, ";");
}

static void
gencode_null(void) {
    fprintf(OUT, "null");    
}   

static void
gencode_obj(m1_object *obj) {
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
        eval_obj(obj->next);   
    }
}

static void
gencode_while(m1_whileexpr *w) {
    fprintf(OUT, "while (");
    gencode_expr(w->cond);
    fprintf(OUT, ") {\n");
    eval_expr(w->cond);
    fprintf(OUT, "}\n");   
}

static void
gencode_dowhile(m1_whileexpr *w) {
    fprintf(OUT, "do {\n");
    gencode_expr(w->block);
    fprintf(OUT, "}\n");       
    fprintf(OUT, "while (");
    eval_expr(w->cond);
    fprintf(OUT, ");\n");
}

static void
gencode_for(m1_forexpr *i) {
    fprintf(OUT, "for (");
    if (i->init)
        gencode_expr(i->init);

    if (i->cond)
        gencode_expr(i->cond);
    fprintf(OUT, ";");
    if (i->step)
        gencode_expr(i->step);
    fprintf(OUT, ") {\n");
    if (i->block)
        gencode_expr(i->block);
    fprintf(OUT, "}");
}

static void
gencode_if(m1_ifexpr *i) {
    fprintf(OUT, "if (");
    gencode_expr(i->cond);
    fprintf(OUT, ") {\n");
    gencode_expr(i->ifblock);
    fprintf(OUT, "}\n");
    if (i->elseblock) {
        fprintf(OUT, "else {\n");
        gencode_expr(i->elseblock);
        fprintf(OUT, "}");
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
    fprintf(OUT, "return ");
    gencode_expr(e);
    fprintf(OUT, ";");    
}

static void
gencode_binary(m1_binexpr *b) {
    char *op;
    gencode_expr(b->left);
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
            op = "&";
            break;
        case OP_BOR:
            op = "|";
            break;
        default:
            op = "unknown op";
            break;   
    }
    fprintf(OUT, "%s", op);
    gencode_expr(b->right);    
}

static void
gencode_unary(m1_unexpr *u) {
    char *op;
    int postfix = 0;
    
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
    if (postfix != 0) {
        gencode_expr(u->expr); 
        fprintf(OUT, "%s", op);
    }
    else {

        fprintf(OUT, "%s", op);
        gencode_expr(u->expr);             
    }
    
    fprintf(OUT, ";");
}

static void
gencode_break(void) {
    fprintf(OUT, "break;");   
}

static void
gencode_funcall(m1_funcall *f) {
    fprintf(OUT, "%s();", f->name);   
}

static void
gencode_expr(m1_expression *e) {
    if (e == NULL) 
        return;
        
    switch (e->type) {
        case EXPR_NUMBER:
            gencode_number(e->expr.floatval);
            break;
        case EXPR_INT:
            gencode_int(e->expr.intval);
            break;
        case EXPR_BINARY:
            gencode_binary(e->expr.b);
            break;
        case EXPR_UNARY:
            gencode_unary(e->expr.u);
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
        default:
            fprintf(stderr, "unknown expr type");   
            exit(EXIT_FAILURE);
    }   

}

static void 
gencode_chunk(m1_chunk *c) {
    m1_expression *iter = c->block;
    fprintf(OUT, ".chunk '%s'\n", c->name);
    while (iter != NULL) {
        gencode_expr(iter);
        iter = iter->next;
        fprintf(OUT, "\n");
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



