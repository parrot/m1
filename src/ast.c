/*

AST node constructors

*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "ast.h"
#include "symtab.h"
#include "compiler.h"


#include "ann.h"


static void expr_set_unexpr(M1_compiler *comp, m1_expression *node, m1_expression *exp, m1_unop op);             
static void expr_set_expr(m1_expression *node, m1_expression *expr);
static void expr_set_obj(m1_expression *node, m1_object *obj);


static void *
m1_malloc(size_t size) {
    void *mem = calloc(1, size);
    if (mem == NULL) {
        fprintf(stderr, "Failed to allocate mem!\n");
        exit(EXIT_FAILURE);
    }
    return mem;   
}


/*@modiefies nothing @*/ 
m1_chunk *
chunk(M1_compiler *comp, char *rettype, NOTNULL(char *name)) {
    m1_chunk *c = (m1_chunk *)m1_malloc(sizeof(m1_chunk));
    c->rettype  = rettype;
    c->name     = name;
    c->block    = NULL;
    c->next     = NULL;
    
    assert(comp != NULL);
    
    /* reset comp->constindex to 0 for each chunk. For each chunk, start
       indexing its constants segment at 0.
     */
    comp->constindex = 0;
    
    return c;   
}

m1_expression *
block(M1_compiler *comp) {
    m1_expression *expr = expression(comp, EXPR_BLOCK);
    expr->expr.blck = (m1_block *)m1_malloc(sizeof(m1_block)); 
    init_symtab(&expr->expr.blck->locals);
    return expr;   
}

void 
block_set_stat(m1_expression *block, m1_expression *stat) {
    block->expr.blck->stats = stat;   
}    

m1_expression *
expression(M1_compiler *comp, m1_expr_type type) {
    m1_expression *e;
    
    assert(comp != NULL);
    assert(comp->yyscanner != NULL);
    
    e        = (m1_expression *)m1_malloc(sizeof(m1_expression));
    e->type  = type;
    /* set the current line number for error reporting. */
    e->line  = yyget_lineno(comp->yyscanner);
    return e;   
}


static m1_literal *
new_literal(m1_valuetype type) {
    m1_literal *l = (m1_literal *)m1_malloc(sizeof(m1_literal));
    l->type       = type;
    return l;    
}


m1_expression *
character(M1_compiler *comp, char ch) {
    m1_expression *expr      = expression(comp, EXPR_CHAR);
    expr->expr.l             = new_literal(VAL_INT);
    expr->expr.l->value.ival = (int)ch;
    expr->expr.l->sym        = sym_enter_int(comp, &comp->currentchunk->constants, (int)ch);
    return expr;    
}

m1_expression *
number(M1_compiler *comp, double value) {
	m1_expression *expr = expression(comp, EXPR_NUMBER);
	
	/* make a new literal node */
	expr->expr.l             = new_literal(VAL_FLOAT);
    expr->expr.l->value.fval = value;
    /* store the constant in the constants segment. */
    expr->expr.l->sym        = sym_enter_num(comp, &comp->currentchunk->constants, value);   
    
	return expr;	
}

m1_expression *
integer(M1_compiler *comp, int value) {
	m1_expression *expr = expression(comp, EXPR_INT);
    /* make a new literal node. */
	expr->expr.l             = new_literal(VAL_INT);
    expr->expr.l->value.ival = value;
    /* store the constant in the constants segment. */
    expr->expr.l->sym        = sym_enter_int(comp, &comp->currentchunk->constants, value);

	return expr;	
}

m1_expression *
string(M1_compiler *comp, char *str) {
	m1_expression *expr = expression(comp, EXPR_STRING);
	assert(str != NULL);

    expr->expr.l = new_literal(VAL_STRING);
    expr->expr.l->value.sval = str;
    
    assert(comp != NULL);
    assert(comp->currentchunk != NULL);
    assert(&comp->currentchunk->constants != NULL);
    
    /* store the string in the constants segment. */
    expr->expr.l->sym = sym_enter_str(comp, &comp->currentchunk->constants, str);

	return expr;	
}

static void
obj_set_index(m1_object *node, m1_expression *index) {
    node->obj.index = index; 
}

m1_object *
arrayindex(M1_compiler *comp, m1_expression *index) {
	m1_object *obj = object(comp, OBJECT_INDEX);
    obj_set_index(obj, index);	
	return obj;	
}

m1_object *
objectfield(M1_compiler *comp, char *field) {
	m1_object *obj = object(comp, OBJECT_FIELD);
	obj_set_ident(obj, field);
	return obj;	
}

m1_object *
objectderef(M1_compiler *comp, char *field) {
	m1_object *obj = object(comp, OBJECT_DEREF);
	obj_set_ident(obj, field);
	return obj;	
}
               
m1_expression *
unaryexpr(M1_compiler *comp, m1_unop op, m1_expression *e) {
	m1_expression *expr = expression(comp, EXPR_UNARY);
	expr_set_unexpr(comp, expr, e, op);
	return expr;
}	
                
m1_expression *
binexpr(M1_compiler *comp, m1_expression *e1, int op, m1_expression *e2) {
	m1_expression *expr = expression(comp, EXPR_BINARY);
	expr->expr.b = (m1_binexpr *)m1_malloc(sizeof(m1_binexpr));
    expr->expr.b->op    = (m1_binop)op;
    expr->expr.b->left  = e1;
    expr->expr.b->right = e2;       
    return expr;    
}

m1_expression *
printexpr(M1_compiler *comp, m1_expression *e) {
	m1_expression *expr = expression(comp, EXPR_PRINT);
	expr_set_expr(expr, e);
	return expr;	
}

static m1_unexpr *
unexpr(M1_compiler *comp, m1_expression *node, m1_unop op) {
    m1_unexpr *e = (m1_unexpr *)m1_malloc(sizeof(m1_unexpr));
    e->expr      = node;
    e->op        = op;    
    
    assert(comp != NULL);
    
    return e;   
}

static void
expr_set_unexpr(M1_compiler *comp, m1_expression *node, m1_expression *exp, m1_unop op) {
    assert(node->type == EXPR_UNARY);   
    node->expr.u = unexpr(comp, exp, op);
}

m1_expression *
funcall(M1_compiler *comp, m1_object *fun, m1_expression *args) {
	m1_expression *expr     = expression(comp, EXPR_FUNCALL);
	expr->expr.f            = (m1_funcall *)m1_malloc(sizeof(m1_funcall));
	
	
    expr->expr.f->name      = fun->obj.name;
//	expr->expr.f->name      = name;
    expr->expr.f->arguments = args;	
    
	/* enter name of function to invoke into constant table. */
	// replace this somehow. Get access to the vtable of the object and copy the
	// method reference from that into this chunk's const segment.
	sym_enter_chunk(comp, &comp->currentchunk->constants, fun->obj.name);
    return expr;   
}


static m1_const *
const_decl(char *type, char *name, m1_expression *expr) {
    m1_const *c = (m1_const *)m1_malloc(sizeof(m1_const));
    c->type     = type;
    c->name     = name;
    c->value    = expr;
    return c;    
}



m1_expression *
constdecl(M1_compiler *comp, char *type, char *name, m1_expression *e) {
	m1_expression *expr = expression(comp, EXPR_CONSTDECL);
	expr->expr.c = const_decl(type, name, e);
	return expr;	
}

static void 
expr_set_for(m1_expression *node, m1_expression *init,
             m1_expression *cond, m1_expression *step,
             m1_expression *stat) 
{
    node->expr.o = (m1_forexpr *)m1_malloc(sizeof(m1_forexpr));
    
    node->expr.o->init  = init;
    node->expr.o->cond  = cond;
    node->expr.o->step  = step;
    node->expr.o->block = stat;
}   

m1_expression *
forexpr(M1_compiler *comp, m1_expression *init, m1_expression *cond, m1_expression *step, m1_expression *stat) {
	m1_expression *expr = expression(comp, EXPR_FOR);
	expr_set_for(expr, init, cond, step, stat);	
	return expr;
}



m1_expression *
inc_or_dec(M1_compiler *comp, m1_expression *obj, m1_unop optype) {	
	m1_expression *expr = expression(comp, EXPR_UNARY);
	expr_set_unexpr(comp, expr, obj, optype);
	return expr;
}

m1_expression *
assignexpr(M1_compiler *comp, m1_expression *lhs, int assignop, m1_expression *rhs) {
	m1_expression *node = expression(comp, EXPR_ASSIGN); 
	
    /*
	a = b  => normal case
	a += b => a = a + b
	*/
    node->expr.a      = (m1_assignment *)m1_malloc(sizeof(m1_assignment));
    node->expr.a->lhs = lhs->expr.t; /* unwrap the m1_object representing lhs from its m1_expression wrapper. */
    
    switch (assignop) {
    	case OP_ASSIGN: /* normal case, lhs = rhs. */
    		node->expr.a->rhs = rhs;        
    		break;
    	default: /* all other cases, such as: 
    	            a +=b => a = a + b; 
    	            make a new binary expression node for a + b.
    	          */
    		node->expr.a->rhs = binexpr(comp, lhs, assignop, rhs);
    		break;
    }


    return node;
}

m1_expression *
objectexpr(M1_compiler *comp, m1_object *obj, m1_expr_type type) {
	m1_expression *expr = expression(comp, type);
	expr_set_obj(expr, obj); 
	return expr;
}

m1_expression *
returnexpr(M1_compiler *comp, m1_expression *retexp) {
	m1_expression *expr = expression(comp, EXPR_RETURN);
	expr_set_expr(expr, retexp);
	return expr;	
}

static void 
expr_set_while(M1_compiler *comp, m1_expression *node, m1_expression *cond, m1_expression *block) {
    assert(comp != NULL);
    node->expr.w        = (m1_whileexpr *)m1_malloc(sizeof(m1_whileexpr));    
    node->expr.w->cond  = cond;
    node->expr.w->block = block;                            
}   

static void
expr_set_if(M1_compiler *comp, m1_expression *node, m1_expression *cond, 
            m1_expression *ifblock, m1_expression *elseblock) 
{
    assert(comp != NULL);
    node->expr.i = (m1_ifexpr *)m1_malloc(sizeof(m1_ifexpr));              
    node->expr.i->cond      = cond;
    node->expr.i->ifblock   = ifblock;
    node->expr.i->elseblock = elseblock;
}

static void 
expr_set_expr(m1_expression *node, m1_expression *expr) {
    node->expr.e = expr;   
}

static void 
expr_set_obj(m1_expression *node, m1_object *obj) {
    node->expr.t = obj;    
}



void 
obj_set_ident(m1_object *node, char *ident) {
    node->obj.name = ident;    
}


m1_object *
object(M1_compiler *comp, m1_object_type type) {
    m1_object *obj = (m1_object *)m1_malloc(sizeof(m1_object));
    obj->type      = type;
    
    assert(comp != NULL);
    return obj;    
}

m1_object *
lhsobj(M1_compiler *comp, m1_object *parent, m1_object *field) {
    m1_object *lhsobj = (m1_object *)m1_malloc(sizeof(m1_object));
    lhsobj->type      = OBJECT_LINK;
    
    lhsobj->obj.field = field;
    lhsobj->parent    = parent;
    
    assert(comp != NULL);
    return lhsobj;   
}

m1_structfield *
structfield(M1_compiler *comp, char *name, char *type) {
    m1_structfield *fld = (m1_structfield *)m1_malloc(sizeof(m1_structfield));
    fld->name           = name;
    fld->type           = type;
    
    assert(comp != NULL);
    return fld;   
}

m1_struct *
newstruct(M1_compiler *comp, char *name, m1_structfield *fields) {
    m1_struct *str = (m1_struct *)m1_malloc(sizeof(m1_struct));    
    str->name      = name;
    str->fields    = fields;
    
    /* struct fields are linked in reverse order, so the first one is
       in fact the one last added, and therefore has the highest offset.
       The offset of the last field is the size of the whole struct
       except the size of the last field itself, so add that.
     */
    str->size = fields->offset + field_size(fields);
    
    assert(comp != NULL);
    return str;   
}

m1_pmc *
newpmc(M1_compiler *comp, char *name, m1_structfield *fields, m1_chunk *methods) {
    m1_pmc *pmc  = (m1_pmc *)m1_malloc(sizeof(m1_pmc));
    
    pmc->name    = name; 
    pmc->fields  = fields;
    pmc->methods = methods;
    
    assert(comp != NULL);
    return pmc;    
}

m1_enum *
newenum(M1_compiler *comp, char *name, m1_enumconst *enumconstants) {
    m1_enum *en  = (m1_enum *)m1_malloc(sizeof(m1_enum));
    en->enumname = name;
    en->enums    = enumconstants;
    
    assert(comp != NULL);
    return en;   
}


m1_expression *
vardecl(M1_compiler *comp, char *type, m1_var *v) {
	m1_expression *expr = expression(comp, EXPR_VARDECL);

	assert(type != NULL);
	assert(v != NULL);
	
	expr->expr.v  = v;

	return expr;	
}

static m1_var *
make_var(M1_compiler *comp, char *varname, m1_expression *init, unsigned num_elems) {
    m1_var *v    = (m1_var *)m1_malloc(sizeof(m1_var));
    v->name      = varname;
    v->type      = comp->parsingtype;
    v->init      = init;
    v->num_elems = num_elems;
    
    /* enter this var. declaration into the symbol table; 
       store a pointer to the symbol in this var. */
    v->sym       = sym_new_symbol(comp, 
                                  comp->currentsymtab, 
                                  varname, 
                                  comp->parsingtype, 
                                  num_elems);
	assert(v->sym != NULL);
    v->sym->var = v; /* set pointer in the symbol (stored in symtab) to the var AST node. */
    return v;    	
}

m1_var *
var(M1_compiler *comp, char *varname, m1_expression *init) {
    /* a single var is just size 1. */
	m1_var *v = make_var(comp, varname, init, 1);
   		
	/* get a pointer to the type declaration */	
	v->sym->typedecl = type_find_def(comp, comp->parsingtype);

    assert(v->sym->typedecl != NULL); 		
    
	return v;
}

/*

Parameters are also represented by m1_var nodes.

*/
m1_var *
parameter(M1_compiler *comp, char *paramtype, char *paramname) {
    m1_var *p = (m1_var *)m1_malloc(sizeof(m1_var));
    p->type   = paramtype;   	                        
    p->name   = paramname;
    /* cannot enter into a symbol table, as there is not yet an active symbol table. */
    return p;
}

void
enter_param(M1_compiler *comp, m1_var *parameter) {
    fprintf(stderr, "Entering param '%s'\n", parameter->name);
    assert(parameter->type != NULL);
    assert(parameter->name != NULL);
    parameter->sym = sym_new_symbol(comp, 
                                    comp->currentsymtab, 
                                    parameter->name,
                                    parameter->type, 
                                    1);       

                                        
    parameter->sym->typedecl = type_find_def(comp, parameter->type);
                                    
    assert(parameter->sym != NULL);                                    
}

m1_var *
array(M1_compiler *comp, char *varname, unsigned num_elems, m1_expression *init) {
    m1_var *v = make_var(comp, varname, init, num_elems);
	return v;
}

m1_expression *
ifexpr(M1_compiler *comp, m1_expression *cond, m1_expression *ifblock, m1_expression *elseblock) {
	m1_expression *expr = expression(comp, EXPR_IF);
	expr_set_if(comp, expr, cond, ifblock, elseblock);
	return expr;
}

m1_expression *
whileexpr(M1_compiler *comp, m1_expression *cond, m1_expression *block) {
	m1_expression *expr = expression(comp, EXPR_WHILE);
	expr_set_while(comp, expr, cond, block);
	return expr;	
}

m1_expression *
dowhileexpr(M1_compiler *comp, m1_expression *cond, m1_expression *block) {
	m1_expression *expr = expression(comp, EXPR_DOWHILE);
	expr_set_while(comp, expr, cond, block);
	return expr;	
}

unsigned 
field_size(struct m1_structfield *field) {
    assert(field != NULL);
	/*
	switch (field->type) {
		case VAL_INT:
			return 4;
		case VAL_FLOAT:
			return 8;
		case VAL_STRING: 
			return 4;
		case VAL_CHUNK: 
			return 4;
        case VAL_ADDRESS:
            return 4;
		default: 
			return 4; 
	}
	*/	
	return 4;
}

static void
expr_set_switch(m1_expression *node, m1_expression *selector, m1_case *cases, m1_expression *defaultstat) {
	node->expr.s = (m1_switch *)m1_malloc(sizeof(m1_switch));
	node->expr.s->selector    = selector; 
	node->expr.s->cases       = cases;
	node->expr.s->defaultstat = defaultstat;
	
}

m1_expression *
switchexpr(M1_compiler *comp, m1_expression *selector, m1_case *cases, m1_expression *defaultstat) {
	m1_expression *node = expression(comp, EXPR_SWITCH);
	expr_set_switch(node, selector, cases, defaultstat); 
	return node;
}

m1_case *
switchcase(M1_compiler *comp, int selector, m1_expression *block) {
	m1_case *c  = (m1_case *)m1_malloc(sizeof(m1_case));
	c->selector = selector;
	c->block    = block;
	c->next     = NULL;
	
    assert(comp != NULL);	
    
	return c;
}

m1_expression *
newexpr(M1_compiler *comp, char *type, m1_expression *args) {
	m1_expression *expr = expression(comp, EXPR_NEW);
	expr->expr.n        = (m1_newexpr *)m1_malloc(sizeof(m1_newexpr));
	expr->expr.n->type  = type;
	expr->expr.n->args  = args;
	return expr;	
}

m1_expression *
castexpr(M1_compiler *comp, char *type, m1_expression *castedexpr) {
    m1_expression *expr = expression(comp, EXPR_CAST);
    m1_castexpr *cast   = (m1_castexpr *)m1_malloc(sizeof(m1_castexpr));
    cast->type          = type;
    cast->expr          = castedexpr;
    expr->expr.cast     = cast;
    return expr;   
}

m1_enumconst *
enumconst(M1_compiler *comp, char *enumitem, int enumvalue) {
    m1_enumconst *ec = (m1_enumconst *)m1_malloc(sizeof(m1_enumconst));
    ec->name         = enumitem;
    ec->value        = enumvalue;
    ec->next         = NULL;
    assert(comp != NULL);
    return ec;       
}


m1_structfield *
struct_find_field(M1_compiler *comp, m1_struct *structdef, char *fieldname) {
    m1_structfield *sfield = NULL;
    
    assert(comp != NULL);
    assert(structdef != NULL);
    assert(fieldname != NULL);
    
    sfield = structdef->fields;
    
    /* go through list of struct's member fields, and see whether <fieldname> is present. */    
    while (sfield != NULL) {
        if (strcmp(sfield->name, fieldname) == 0) { /* found! */        
            return sfield;
        }
        sfield = sfield->next;   
    }
    return sfield;        
}


/*

Open a new scope. This function returns a new m1_block object,
which holds its own symboltable that will store all variables
declared in this scope. Set comp's currentsymtab to this scope's
symbol table.

*/
m1_expression *
open_scope(M1_compiler *comp) {    
    m1_expression *exp = block(comp);
    
    /* link to current symbol table, which is the parent scope. */    
    exp->expr.blck->locals.parentscope = comp->currentsymtab;
    comp->currentsymtab = &exp->expr.blck->locals;

    return exp;
}

/*

Close the current scope. Comp's current symboltable is set to the
previous (parent) scope. 

*/
void
close_scope(M1_compiler *comp) {
    comp->currentsymtab = comp->currentsymtab->parentscope;
}

