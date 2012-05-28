/*

Code generator.

Visit each node, and generate instructions as appropriate.
See m1_ast.h for an overview of the AST node types. For most
nodes/functions, a m1_reg structure is returned, that holds the
type and number of the register that will hold the result of
the expression for which code was generated.

Example: a node representing a floating point number will load
the number in an N register, and return that register. This happens
in gencode_number().

*/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include "m1_gencode.h"
#include "m1_ast.h"
#include "m1_compiler.h"
#include "m1_stack.h"
#include "m1_symtab.h"

#include "m1_ann.h"

#define OUT	stdout

#define M1DEBUG 1

#ifdef M1DEBUG
    #define debug(x)    fprintf(stderr, x);
#else
    #define debug(x)
#endif


static m1_reg gencode_expr(M1_compiler *comp, m1_expression *e);
static void gencode_block(M1_compiler *comp, m1_expression *block);


static const char type_chars[4] = {'i', 'n', 's', 'p'};
static const char reg_chars[4] = {'I', 'N', 'S', 'P'};


/*

Allocate new registers as needed.

*/
static m1_reg
gen_reg(M1_compiler *comp, m1_valuetype type) {
    m1_reg reg;
    
    assert(comp != NULL);
    assert((type >=0) && (type < 4));
    
    reg.type = type;
	reg.no   = comp->regs[type]++;   
    fprintf(stderr, "generating regi %d for type %d\n", reg.no, type);
    
    return reg;
}

/*

Generate label identifiers.

*/
static int
gen_label(M1_compiler *comp) {
    assert(comp != NULL);
	return comp->label++;	
}


static m1_reg
gencode_number(M1_compiler *comp, m1_literal *lit) {
	/*
	deref Nx, CONSTS, <const_id>
	*/
    m1_reg     reg,
               constindex;

    
    assert(comp != NULL);
    assert(lit != NULL);
    assert(lit->type = VAL_FLOAT);
    assert(lit->sym != NULL);
       
    reg        = gen_reg(comp, VAL_FLOAT);
    constindex = gen_reg(comp, VAL_INT);
   
        
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", constindex.no, lit->sym->constindex);
    fprintf(OUT, "\tderef\tN%d, CONSTS, %d\n", reg.no, constindex.no);
    return reg;
}   

static m1_reg
gencode_int(M1_compiler *comp, m1_literal *lit) {
	/*
	
	
	deref Ix, CONSTS, <const_id>
	
	or 
	
	set_imm Ix, y, z
	
	*/
    m1_reg     reg;

    assert(comp != NULL);
    assert(lit != NULL);
    assert(lit->type == VAL_INT);
    assert(lit->sym != NULL);

    reg = gen_reg(comp, VAL_INT);   
    
    if (lit->sym->value.ival < (256 * 255)) { /* XXX check these numbers. operands are 8 bit? */
        /* use set_imm X, N*256, remainder)   */
        int remainder = lit->sym->value.ival % 256;
        int num256    = (lit->sym->value.ival - remainder) / 256; 
        fprintf(OUT, "\tset_imm\tI%d, %d, %d\n", reg.no, num256, remainder);
    } 
    else {
        m1_reg constindex = gen_reg(comp, VAL_INT);

        fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", constindex.no, lit->sym->constindex);
        fprintf(OUT, "\tderef\tI%d, CONSTS, I%d\n", reg.no, constindex.no);
    }
    return reg;
}

static m1_reg
gencode_string(M1_compiler *comp, m1_literal *lit) {
    m1_reg     reg,
               constindex;
    
    assert(comp != NULL);
    assert(lit != NULL);
    assert(lit->sym != NULL);
    assert(lit->type == VAL_STRING);
   
    reg        = gen_reg(comp, VAL_STRING);
    constindex = gen_reg(comp, VAL_INT);
      
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", constindex.no, lit->sym->constindex);
    fprintf(OUT, "\tderef\tS%d, CONSTS, I%d\n", reg.no, constindex.no);
    return reg;
}


static m1_reg
gencode_assign(M1_compiler *comp, NOTNULL(m1_assignment *a)) {
	m1_reg lhs, rhs;
	
	debug("gencode_assign start...\n");
	
	assert(a != NULL);
	
    rhs = gencode_expr(comp, a->rhs);
    lhs = gencode_expr(comp, a->lhs);
    /* copy the value held in register for rhs to the register of lhs */
    assert((lhs.type >= 0) && (lhs.type < 4));
    assert((rhs.type >= 0) && (rhs.type < 4));
    
    fprintf(OUT, "\tset\t%c%d, %c%d, x\n", reg_chars[(int)lhs.type], lhs.no, reg_chars[(int)rhs.type], rhs.no);
    
    
	return lhs;
}

static m1_reg
gencode_null(M1_compiler *comp) {
	m1_reg reg;

    return reg;
}   

static m1_reg
gencode_obj(M1_compiler *comp, m1_object *obj) {
	m1_reg reg;
	

	assert(comp != NULL);
	assert(comp->currentchunk != NULL);
	assert(&comp->currentchunk->locals != NULL);
	
	
	//oreg = gen_reg(comp, VAL_INT);
	
	
    switch (obj->type) {
        case OBJECT_MAIN: 
        {        	
        	assert(obj->obj.field != NULL);
        	assert(obj->sym != NULL);

        	/* if symbol has not register allocated yet, do it now. */
        	if (obj->sym->regno == NO_REG_ALLOCATED_YET) {
                m1_reg r = gen_reg(comp, obj->sym->valtype); 
                obj->sym->regno = r.no;
        	}  
        	reg.no   = obj->sym->regno;
        	reg.type = obj->sym->valtype; 

            break;
        }
        case OBJECT_FIELD: /* b in a.b */
        {
            /* TODO */
            /*
            m1_struct *s = find_struct(comp, 
            m1_structfield *iter = 
            while () {
            obj->obj.field   
            }
            */
            break;
        }
        case OBJECT_DEREF: /* b in a->b */
        	/* todo */
            break;
        case OBJECT_INDEX: /* b in a[b] */        
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
	
	   ...
	   goto L2
	L1
	  <block>
	
	L2:
	   goto_if <cond>, L1
	   ...
	
	*/
	m1_reg reg;
	int startlabel = gen_label(comp), 
	    endlabel   = gen_label(comp);
	
	/* push break label onto stack so break statement knows where to go. */
	push(comp->breakstack, endlabel);
	
	fprintf(OUT, "\tgoto L%d\n", endlabel);
	
	fprintf(OUT, "L%d:\n", startlabel);
	gencode_block(comp, w->block);
	
	fprintf(OUT, "L%d:\n", endlabel);
	reg = gencode_expr(comp, w->cond);
	fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", startlabel, reg_chars[(int)reg.type], reg.no);
			
	/* remove break label from stack. */
	(void)pop(comp->breakstack);
	
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
    
    int startlabel = gen_label(comp);
    int endlabel   = gen_label(comp);
    
    push(comp->breakstack, endlabel);
     
    fprintf(OUT, "L%d:\n", startlabel);
    gencode_block(comp, w->block);
    
    reg = gencode_expr(comp, w->cond);
    fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", startlabel, reg_chars[(int)reg.type], reg.no);

    fprintf(OUT, "L%d:\n", endlabel);
    
    (void)pop(comp->breakstack);
    
    return reg;
}

static m1_reg
gencode_for(M1_compiler *comp, m1_forexpr *i) {
	/*
	
	
	<code for init>
	START:
	<code for cond>
	goto_if cond, L1
	goto END
	L1: 
	<code for block>
	<code for step>
	goto START
	END:
	*/
    int startlabel = gen_label(comp),
        endlabel   = gen_label(comp),
        blocklabel = gen_label(comp);
        
    m1_reg reg;
    
    if (i->init)
        gencode_expr(comp, i->init);

	fprintf(OUT, "L%d:\n", startlabel);
	
    if (i->cond)
        reg = gencode_expr(comp, i->cond);
   
    fprintf(OUT, "\tgoto_if L%d, %c%d\n", blocklabel, reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "\tgoto L%d\n", endlabel);
    
    fprintf(OUT, "L%d:\n", blocklabel);
    
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
	goto_if result1, L1
	<code for elseblock>
	goto L2
    L1:
	<code for ifblock>
	L2:
	*/
	m1_reg condreg;
	int endlabel = gen_label(comp),
		iflabel  = gen_label(comp) ;

	
    condreg = gencode_expr(comp, i->cond);
	
	fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", iflabel, reg_chars[(int)condreg.type], condreg.no);

    /* else block */
    if (i->elseblock) {            	
        gencode_block(comp, i->elseblock);     
    }
    fprintf(OUT, "\tgoto L%d\n", endlabel);
    
    /* if block */
	fprintf(OUT, "L%d:\n", iflabel);
    gencode_expr(comp, i->ifblock);
			
    fprintf(OUT, "L%d:\n", endlabel);
    return condreg;       
}

static m1_reg
gencode_deref(M1_compiler *comp, m1_object *o) {
	m1_reg reg;

    reg = gencode_obj(comp, o);
    return reg;
}

static m1_reg
gencode_address(M1_compiler *comp, m1_object *o) {
	m1_reg reg;

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
	  left = <evaluate left>
	  goto_if left, END
	  right = <evaluate right>
	  left = right 
	END:
	*/
	m1_reg left, right;
	int endlabel;
	
	endlabel  = gen_label(comp);
	
	
	left = gencode_expr(comp, b->left);	
	/* if left was not true, then need to evaluate right, otherwise short-cut. */
	fprintf(OUT, "\tgoto_if L%d, %c%d\n", endlabel, reg_chars[(int)left.type], left.no);
	right = gencode_expr(comp, b->right);	
	fprintf(OUT, "\tset\t%c%d, %c%d, x\n", reg_chars[(int)left.type], left.no, 
	                                       reg_chars[(int)right.type], right.no);
	fprintf(OUT, "L%d:\n", endlabel);
	return left;	
}

static m1_reg
gencode_and(M1_compiler *comp, m1_binexpr *b) {
	/*
	  left = <evaluate left>
	  goto_if left, evalright
	  goto END
	evalright:
	  right = <evaluate right>
	  left = right
	END:
	*/
	m1_reg left, right;
	int endlabel  = gen_label(comp);
	int evalright = gen_label(comp);
	
	left = gencode_expr(comp, b->left);
	/* if left was false, no need to evaluate right, and go to end. */
	fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", evalright, reg_chars[(int)left.type], left.no);		
	fprintf(OUT, "\tgoto L%d\n", endlabel);
	fprintf(OUT, "L%d:\n", evalright);
	right = gencode_expr(comp, b->right);
	/* copy result from right to left result reg, as that's the reg that will be returned. */
	fprintf(OUT, "\tset\t%c%d, %c%d, x\n", reg_chars[(int)left.type], left.no, reg_chars[(int)right.type], right.no);	
	fprintf(OUT, "L%d:\n", endlabel);
	
	return left;
}

static m1_reg
gencode_ne(M1_compiler *comp, m1_binexpr *b) {
    m1_reg reg, left, right;
    int endlabel, equal_label;
    
    left  = gencode_expr(comp, b->left);
    right = gencode_expr(comp, b->right);
    endlabel    = gen_label(comp);
    equal_label = gen_label(comp);
    
    reg = gen_reg(comp, VAL_INT);
    fprintf(OUT, "\tsub_i\tI%d, %c%d, %c%d\n", reg.no, reg_chars[(int)left.type], left.no,
                                                      reg_chars[(int)right.type], right.no);
                                                      
    fprintf(OUT, "\tgoto_if L%d, %c%d\n", equal_label, reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "\tset_imm\t%c%d, 0, 0\n", reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "\tgoto L%d\n", endlabel);                                                      
    
    fprintf(OUT, "L%d:\n", equal_label);
    fprintf(OUT, "\tset_imm\t%c%d, 0, 1\n", reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "L%d:\n", endlabel);    
    
    return reg;   
}

static m1_reg
gencode_eq(M1_compiler *comp, m1_binexpr *b) {
    /*
    left = <code for left>
    right = <code for right>
    diff = left - right
    goto_if NOTEQUAL, diff # not zero
    result = 1
    goto END
    NOTEQUAL:
    result = 0
    END:
    
    */
    m1_reg reg, left, right;
    int endlabel, notequal_label;
    
    left  = gencode_expr(comp, b->left);
    right = gencode_expr(comp, b->right);
    endlabel       = gen_label(comp);
    notequal_label = gen_label(comp);
    
    reg = gen_reg(comp, VAL_INT);
    fprintf(OUT, "\tsub_i\tI%d, %c%d, %c%d\n", reg.no, reg_chars[(int)left.type], left.no,
                                                      reg_chars[(int)right.type], right.no);
                                                      
    fprintf(OUT, "\tgoto_if L%d, %c%d\n", notequal_label, reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "\tset_imm\t%c%d, 0, 1\n", reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "\tgoto L%d\n", endlabel);                                                      
    
    fprintf(OUT, "L%d:\n", notequal_label);
    fprintf(OUT, "\tset_imm\t%c%d, 0, 0\n", reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "L%d:\n", endlabel);
    
    return reg;   
}

static m1_reg
gencode_binary(M1_compiler *comp, m1_binexpr *b) {
    char  *op = NULL;
    m1_reg left, 
    	   right,
    	   target;
    
    left = gencode_expr(comp, b->left);

    switch(b->op) {
    	case OP_ASSIGN:
    		op = "set"; /* in case of a = b = c; then b = c part is a binary expression */
    		break;
        case OP_PLUS:
            if (left.type == VAL_INT)
                op = "add_i";
            else if (left.type == VAL_FLOAT)
                op = "add_n";
            else { /* should not happen */
                fprintf(stderr, "wrong type for add");
                exit(EXIT_FAILURE);
            }
            break;
        case OP_MINUS:
             if (left.type == VAL_INT)
                op = "sub_i";
            else if (left.type == VAL_FLOAT)
                op = "sub_n";
            else { /* should not happen */
                fprintf(stderr, "wrong type for sub");
                exit(EXIT_FAILURE);
            }

            break;
        case OP_MUL:
             if (left.type == VAL_INT)
                op = "mul_i";
            else if (left.type == VAL_FLOAT)
                op = "mul_n";
            else { /* should not happen */
                fprintf(stderr, "wrong type for mul");
                exit(EXIT_FAILURE);
            }

            break;
        case OP_DIV:
             if (left.type == VAL_INT)
                op = "div_i";
            else if (left.type == VAL_FLOAT)
                op = "div_n";
            else { /* should not happen */
                fprintf(stderr, "wrong type for div");
                exit(EXIT_FAILURE);
            }
            break;
        case OP_MOD:
            if (left.type == VAL_INT)
                op = "mod_i";
            else if (left.type == VAL_FLOAT)
                op = "mod_n";
            else { /* should not happen */
                fprintf(stderr, "wrong type for mod");
                exit(EXIT_FAILURE);
            }
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
            return gencode_eq(comp, b);
            
        case OP_NE: /* a != b;*/
            return gencode_ne(comp, b);

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
    
    right       = gencode_expr(comp, b->right);  
    target      = gen_reg(comp, left.type);  
    
    fprintf(OUT, "\t%s\t%c%d, %c%d, %c%d\n", op, reg_chars[(int)target.type], target.no, 
           reg_chars[(int)left.type], left.no, reg_chars[(int)right.type], right.no);
    return target;
}


static m1_reg
gencode_not(M1_compiler *comp, m1_unexpr *u) {
    m1_reg reg, temp;
    int label1, label2;
    
    reg  = gencode_expr(comp, u->expr);
    temp = gen_reg(comp, VAL_INT);
    
    /* if reg is zero, make it nonzero (false->true).
    If it's non-zero, make it zero. (true->false). 
    */
    /*
      goto_if reg, L1 #non-zero, make it zero.
      set_imm Ix, 0, 0
      goto L2
    L1: # nonzero, make it zero
      set_imm Ix, 0, 1
    L2:
      set reg, Ix
    #done
    
    */
    label1 = gen_label(comp);
    label2 = gen_label(comp);
    
    fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", label1, reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n", temp.no);
    fprintf(OUT, "\tgoto L%d\n", label2);
    fprintf(OUT, "L%d:\n", label1);
    fprintf(OUT, "\tset_imm\tI%d, 0, 0\n", temp.no);
    fprintf(OUT, "L%d:\n", label2);
    fprintf(OUT, "\tset\t%c%d, I%d, x\n", reg_chars[(int)reg.type], reg.no, temp.no);
    
    return temp;
}

static m1_reg
gencode_uminus(M1_compiler *comp, m1_unexpr *u) {
    /*
    x = -x
    (e.g: x = 42 => x = -42).
    */
    m1_reg reg;
    
    reg = gencode_expr(comp, u->expr);
    
    /* XXX M0 definitely needs a gt or lt opcode. 
    a "neg" and "not" as well as bit-inverse opcode (a = ~a) would
    be handy too.
    */
    
    return reg;    
}

static m1_reg
gencode_unary(M1_compiler *comp, NOTNULL(m1_unexpr *u)) {
    char  *op;
    int    postfix = 0;
    m1_reg reg, 
           target, 
           one;
    
    switch (u->op) {
        case UNOP_POSTINC:
            postfix = 1;
            op = "add_i";
            break;
        case UNOP_POSTDEC:
            op = "sub_i";
            postfix = 1;
            break;
        case UNOP_PREINC:
            postfix = 0;
            op = "add_i";
            break;
        case UNOP_PREDEC:
            op = "sub_i";
            postfix = 0; 
            break;
        case UNOP_MINUS:
            return gencode_uminus(comp, u);
        case UNOP_NOT:
            return gencode_not(comp, u);
        default:
            op = "unknown op";
            break;   
    }   
    gen_reg(comp, VAL_INT); /* for final value */
    gen_reg(comp, VAL_INT); /* to store "1" */

    
    /* generate code for the pre/post ++ expression */ 
    reg = gencode_expr(comp, u->expr);
    
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n", one.no);
    fprintf(OUT, "\t%s\tI%d, I%d, I%d\n", op, target.no, reg.no, one.no);
    
    if (postfix == 0) { 
        /* prefix; return reg containing value before adding 1 */
    	reg.no = target.no; 
    	fprintf(OUT, "\tset\tI%d, %Id, x\n", reg.no, target.no);

    }	
    else {
        fprintf(OUT, "\tset\tI%d, I%d, x\n", reg.no, target.no);
    }
        
    return reg;	    
    
}

static void
gencode_break(M1_compiler *comp) {
	
	/* get label to jump to */
	int breaklabel = top(comp->breakstack);
	
	/* pop label from compiler
	's label stack (todo!) and jump there. */
    fprintf(OUT, "\tgoto\tL%d\n", breaklabel);
}

static m1_reg
gencode_funcall(M1_compiler *comp, m1_funcall *f) {
	m1_reg reg;
	m1_reg pmcreg;
	m1_reg offsetreg;
    m1_symbol *fun = sym_find_chunk(&comp->currentchunk->constants, f->name);
    
    if (fun == NULL) {
        fprintf(stderr, "Cant find function '%s'\n", f->name);
        ++comp->errors;
        return reg;
    }
    reg = gen_reg(comp, VAL_INT);
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", reg.no, fun->constindex);
    pmcreg = gen_reg(comp, VAL_CHUNK);
    offsetreg = gen_reg(comp, VAL_INT);
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", offsetreg.no, 0);
    fprintf(OUT, "\tderef\tP%d, CONSTS, I%d\n", pmcreg.no, reg.no);
    fprintf(OUT, "\tgoto_chunk\tP%d, I%d, x\n", pmcreg.no, offsetreg.no);
    return reg;
}


static m1_reg
gencode_print(M1_compiler *comp, m1_expression *expr) {

    m1_reg reg;
    m1_reg one;
    
    reg = gencode_expr(comp, expr);
    one = gen_reg(comp, VAL_INT);
    
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n",  one.no);
	fprintf(OUT, "\tprint_%c\tI%d, %c%d, x\n", type_chars[(int)reg.type], one.no, reg_chars[(int)reg.type], reg.no);
	
	return reg;
}

static m1_reg
gencode_new(M1_compiler *comp, m1_newexpr *expr) {
	m1_reg reg     = gen_reg(comp, VAL_INT);
	m1_reg sizereg = gen_reg(comp, VAL_INT);
	
	unsigned size  = 128; /* fix; should be size of object requested */
	
	fprintf(OUT, "\tset_imm I%d, 0, %d\n", sizereg.no, size);
	fprintf(OUT, "\tgc_alloc\tI%d, I%d, 0\n", reg.no, sizereg.no);
	
	return reg;	
}

static m1_reg
gencode_switch(M1_compiler *comp, m1_switch *expr) {
    m1_reg reg;
    int endlabel = gen_label(comp);
    
    reg = gencode_expr(comp, expr->selector);
    
    push(comp->breakstack, endlabel); /* for break statements to jump to. */    
    /* XXX implement cases */
    
    (void)pop(comp->breakstack);
    
    return reg;   
}

static void
gencode_vardecl(M1_compiler *comp, m1_var *v) {
    
    if (v->init) {
       m1_reg     reg = gencode_expr(comp, v->init);     
       m1_symbol *s   = v->sym;
       
       /* check for first usage of this variable; may not have a reg allocated yet. */
       if (s->regno == NO_REG_ALLOCATED_YET) {
            m1_reg r = gen_reg(comp, s->valtype);
            s->regno = r.no;
       }
       fprintf(OUT, "\tset\t%c%d, %c%d, x\n", reg_chars[s->valtype], 
                                              s->regno, 
                                              reg_chars[(int)reg.type], 
                                              reg.no);
    }
       
}

static m1_reg
gencode_expr(M1_compiler *comp, m1_expression *e) {

    m1_reg reg;
    
    debug("gencode_expr start...\n");
    
    if (e == NULL) {
    	debug("expr e is null in gencode_expr\n");
        return reg;
    }
        
    switch (e->type) {
        case EXPR_NUMBER:
            reg = gencode_number(comp, e->expr.l);
            break;
        case EXPR_INT:
            reg = gencode_int(comp, e->expr.l);
            break;
        case EXPR_STRING:
            reg = gencode_string(comp, e->expr.l);     
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
            debug("expr type is EXPR_OBJECT\n");
            reg = gencode_obj(comp, e->expr.t);
            break;
        case EXPR_BREAK:
            gencode_break(comp);
            break;
        case EXPR_CONSTDECL:
            /* do nothing. constants are compiled away */
        	break;
        case EXPR_VARDECL:
            gencode_vardecl(comp, e->expr.v);            
            break;
        case EXPR_SWITCH:
            reg = gencode_switch(comp, e->expr.s);
        	break;
        case EXPR_NEW:
        	reg = gencode_new(comp, e->expr.n);
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
gencode_consts(m1_symboltable *consttable) {
    m1_symbol *iter;
	
	fprintf(OUT, ".constants\n");
	
    assert(consttable != NULL);
	iter = consttable->syms;
	
	while (iter != NULL) {
		
		switch (iter->valtype) {
			case VAL_STRING:
				fprintf(OUT, "%d %s\n", iter->constindex, iter->value.sval);
				break;
			case VAL_FLOAT:
				fprintf(OUT, "%d %f\n", iter->constindex, iter->value.fval);
				break;
			case VAL_INT:			
				fprintf(OUT, "%d %d\n", iter->constindex, iter->value.ival);
				break;
	        case VAL_CHUNK:
	            fprintf(OUT, "%d &%s\n", iter->constindex, iter->value.sval);
	            break;
			default:
				fprintf(stderr, "unknown symbol type");
				exit(EXIT_FAILURE);
		}
		iter = iter->next;	
	}

}

static void
gencode_metadata(m1_chunk *c) {
	fprintf(OUT, ".metadata\n");	
}

static void
gencode_block(M1_compiler *comp, m1_expression *block) {
    m1_expression *iter = block;
    
    while (iter != NULL) {
        (void)gencode_expr(comp, iter);
        iter = iter->next;
    }
}

static void 
gencode_chunk(M1_compiler *comp, m1_chunk *c) {
    
    int i;
    
#ifdef PRELOAD_0_AND_1    
    m1_reg r0, r1;
    
    /* The numbers 0 and 1 are used quite a lot. Rather than
       generating them as needed, pre-store them in registers
       0 and 1. Small overhead for when it's not needed, but
       saves quite a few instructions overall in more 
       complex code. 
     */
     
    r0 = gen_reg(comp, VAL_INT);
    r1 = gen_reg(comp, VAL_INT); 
    
    fprintf(OUT, "\tset_imm\tI%d, 0, 0\n", r0.no);
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n", r1.no); 

#endif

    fprintf(OUT, ".chunk \"%s\"\n", c->name);    
    /* for each chunk, reset the register allocator */
    for (i = 0; i < NUM_TYPES; ++i)
        comp->regs[i] = 0;
        
    gencode_consts(&c->constants);
    gencode_metadata(c);
    
    fprintf(OUT, ".bytecode\n");    
    
    debug("gencode_block\n");
    /* generate code for statements */
    gencode_block(comp, c->block);
}

/*

Top-level function to drive the code generation phase.
Iterate over the list of chunks, and generate code for each.
*/
void 
gencode(M1_compiler *comp, m1_chunk *ast) {
    m1_chunk *iter = ast;
     
     
    fprintf(OUT, ".version 0\n");
    while (iter != NULL) {        
        gencode_chunk(comp, iter);
        iter = iter->next;   
    }
}



