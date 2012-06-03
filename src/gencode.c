/*

Code generator.

Visit each node, and generate instructions as appropriate.
See ast.h for an overview of the AST node types. For most
nodes/functions, one (and sometimes more) m1_regs are pushed onto
a stack (accessible through the compiler structure parameter), 
that holds the type and number of the register that will hold 
the result of the expression for which code was generated.

Example: a node representing a floating point number will load
the number in an N register, and push that register so that other
functions can access it (i.e., popping it off the stack). 
This happens in gencode_number().

*/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>
#include "gencode.h"
#include "ast.h"
#include "compiler.h"
#include "stack.h"
#include "symtab.h"
#include "decl.h"

#include "ann.h"

#define OUT	stdout

#define M1DEBUG 1

#ifdef M1DEBUG
    #define debug(x)    fprintf(stderr, x);
#else
    #define debug(x)
#endif


static void gencode_expr(M1_compiler *comp, m1_expression *e);
static void gencode_block(M1_compiler *comp, m1_expression *block);
static unsigned gencode_obj(M1_compiler *comp, m1_object *obj, m1_object **parent, int is_target);

static const char type_chars[4] = {'i', 'n', 's', 'p'};
static const char reg_chars[4] = {'I', 'N', 'S', 'P'};


/*

Allocate new registers as needed.

*/
static m1_reg
gen_reg(M1_compiler *comp, m1_valuetype type) {
    m1_reg reg;
    
    assert(comp != NULL);
    assert(type < 4);
    
    reg.type = type;
	reg.no   = comp->regs[type]++;   
 
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


static void
gencode_number(M1_compiler *comp, m1_literal *lit) {
	/*
	deref Nx, CONSTS, <const_id>
	*/
    m1_reg     reg, constindex;
    
    assert(comp != NULL);
    assert(lit != NULL);
    assert(lit->type = VAL_FLOAT);
    assert(lit->sym != NULL);
       
    reg        = gen_reg(comp, VAL_FLOAT);
    constindex = gen_reg(comp, VAL_INT);
   
        
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", constindex.no, lit->sym->constindex);
    fprintf(OUT, "\tderef\tN%d, CONSTS, %d\n", reg.no, constindex.no);
    
    pushreg(comp->regstack, reg);
    
}   

static void
gencode_int(M1_compiler *comp, m1_literal *lit) {
	/*
	If the value is smaller than 256*255 and > 0, 
	then generate set_imm, otherwise, load the constant 
	from the constants table.

	    deref Ix, CONSTS, <const_id>
    or:	
    	set_imm Ix, y, z
	
	*/
    m1_reg reg;

    assert(comp != NULL);
    assert(lit != NULL);
    assert(lit->type == VAL_INT);
    assert(lit->sym != NULL);

    reg = gen_reg(comp, VAL_INT);   
    
    /* If the value is small enough, load it with set_imm; otherwise, take it from the constants table.
       set_imm X, Y, Z: set X to: 256 * Y + Z. All operands are 8 bit, so maximum value is 255. 
     */
    if (lit->sym->value.ival < (256 * 255) && lit->sym->value.ival >= 0) { 
        /* use set_imm X, N*256, remainder)   */
        int remainder = lit->sym->value.ival % 256;
        int num256    = (lit->sym->value.ival - remainder) / 256; 
        fprintf(OUT, "\tset_imm\tI%d, %d, %d\n", reg.no, num256, remainder);
    } 
    else { /* too big enough for set_imm, so load it from constants segment. */
        m1_reg constindex = gen_reg(comp, VAL_INT);

        fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", constindex.no, lit->sym->constindex);
        fprintf(OUT, "\tderef\tI%d, CONSTS, I%d\n", reg.no, constindex.no);
    }
    
    pushreg(comp->regstack, reg);
    
}

static void
gencode_string(M1_compiler *comp, m1_literal *lit) {
    m1_reg reg,
           constidxreg;
    
    assert(comp != NULL);
    assert(lit != NULL);
    assert(lit->sym != NULL);
    assert(lit->type == VAL_STRING);
   
    reg         = gen_reg(comp, VAL_STRING);
    constidxreg = gen_reg(comp, VAL_INT);
      
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", constidxreg.no, lit->sym->constindex);
    fprintf(OUT, "\tderef\tS%d, CONSTS, I%d\n", reg.no, constidxreg.no);
    
    fprintf(stderr, "pushing reg with string on stack: %c%d\n", reg_chars[(int)reg.type], reg.no);
    pushreg(comp->regstack, reg);

}


static void
gencode_assign(M1_compiler *comp, NOTNULL(m1_assignment *a)) {
	m1_reg lhs, rhs;
	m1_object *parent;
	unsigned obj_reg_count;
		
	assert(a != NULL);
	
    gencode_expr(comp, a->rhs);
    rhs = popreg(comp->regstack);
    
    obj_reg_count = gencode_obj(comp, a->lhs, &parent, 1);    
    
    /* the number of registers that are available is always 1 or 2. 1 for the simple case,
       and 2 for field access (x.y and x[1]). 
    */
    assert (obj_reg_count == 1 || obj_reg_count == 2);

    if (obj_reg_count == 1) {
        lhs = popreg(comp->regstack);    
        fprintf(OUT, "\tset \t%c%d, %c%d, x\n", reg_chars[(int)lhs.type], lhs.no, 
                                                reg_chars[(int)rhs.type], rhs.no);
    }
    else if (obj_reg_count == 2) {
        m1_reg index  = popreg(comp->regstack);
        m1_reg parent = popreg(comp->regstack);
        fprintf(OUT, "\tset_ref\t%c%d, %c%d, %c%d\n", reg_chars[(int)parent.type], parent.no, 
                                                      reg_chars[(int)index.type], index.no,
                                                      reg_chars[(int)rhs.type], rhs.no);
    }    
    else {
        assert(0);
    }      
}

static void
gencode_null(M1_compiler *comp) {
	m1_reg reg;
	reg = gen_reg(comp, VAL_INT);
	/* "null" is just 0, but then in a "pointer" context. */
    fprintf(OUT, "\tset_imm\tI%d, 0, 0\n", reg.no);
    
    pushreg(comp->regstack, reg);
}   

static unsigned
gencode_obj(M1_compiler *comp, m1_object *obj, m1_object **parent, int is_target) {

    unsigned numregs_pushed = 0;

	assert(comp != NULL);
    assert(comp->currentchunk != NULL);
    assert(&comp->currentchunk->locals != NULL);
	   
	/* visit this node's parent recursively, depth-first. 
    parent parameter will return a pointer to it so it can
    be passed on when visiting the field. Note that this invocation
    is recursive, so THIS function will be called again. Note also that
    the tree was built upside down, so obj->parent is really its parent
    in which the current node is a (link-node to a) field.
	   
    x.y.z looks like this:
	   
         	 OBJECT_MAIN 
	            |
	            |  OBJECT_FIELD
	            |   |
	            |   |   OBJECT_FIELD
	            V   V   V
	       
	            x   y   z
	             \ /   /
OBJECT_LINK-----> L1  /
	               \ /
OBJECT_LINK-------> L2
	        
	                 ^
	                 |
	                ROOT
	       
    Node L2 is the root in this tree. Both L1 and L2 are of type OBJECT_LINK.
    Node "x" is OBJECT_MAIN, whereas nodes "y" and "z" are OBJECT_FIELD.
    First this function (gencode_obj) goes all the way down to x, sets the
    OUT parameter "parent" to itself, then as the function returns, comes
    back to L1, then visits y, passing on a pointer to node for "x" through
    the parent parameter. Then, node y sets the parent OUT parameter to itself
    (again, in this funciton gencode_obj), and then control goes up to L2,
    visiting z, passing a pointer to node "y" through the parent parameter.
  	  
  	  
  	  
  	x[42] looks like this:
  	
           OBJECT_MAIN
                |    
                |   OBJECT_INDEX
                |    |
                V    V
  	
  	            x   42 
  	             \  /
  	              \/
OBJECT_LINK-----> L1  
  	    
  	              ^
  	              |
  	             ROOT   
    */

    switch (obj->type) {
        case OBJECT_LINK:
        {
            /* set OUT parameter to this node (that's currently visited, obj) */
            *parent = obj;   	

            gencode_obj(comp, obj->parent,parent, is_target);
            numregs_pushed += gencode_obj(comp, obj->obj.field, parent, is_target);     
            
            break;
            
        }
        case OBJECT_MAIN: 
        {   
            m1_reg reg;              

        	assert(obj->obj.field != NULL);
        	assert(obj->sym != NULL);
        	assert(obj->sym->typedecl != NULL);

             
        	/* if symbol has not register allocated yet, do it now. */
        	if (obj->sym->regno == NO_REG_ALLOCATED_YET) {
                m1_reg r = gen_reg(comp, obj->sym->typedecl->valtype); 
                obj->sym->regno = r.no;
        	}  
/*        	else {
                fprintf(stderr, "Reg of %s is %d\n", obj->sym->name, obj->sym->regno);        	
        	}
*/        	
        	reg.no   = obj->sym->regno;
        	reg.type = obj->sym->typedecl->valtype; 

                      
            /* return a pointer to this node by OUT parameter. */
            *parent = obj;
            
            pushreg(comp->regstack, reg);
            ++numregs_pushed;
            
            break;
        }
        case OBJECT_FIELD: /* example: b in a.b */
        {            
            m1_reg          fieldreg;            
            int             offset;                        
            m1_structfield *field;
            
            assert((*parent)->sym != NULL);
            assert((*parent)->sym->typedecl != NULL);
            
            /* pass comp, a pointer to the struct decl of this obj's parent, and this obj's name. */
            field    = struct_find_field(comp, (*parent)->sym->typedecl->d.s, obj->obj.name);
            assert(field != NULL); /* XXX need to check in semcheck. */
            offset   = field->offset;                        
            fieldreg = gen_reg(comp, VAL_INT); /* reg for storing offset of field. */

            /* load the offset into a reg. and make it available through the regstack. */
            fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", fieldreg.no, offset);             
            pushreg(comp->regstack, fieldreg);
            ++numregs_pushed;
            
            
            /* XXX if offset = 0. special case? */
          //  if (offset > 0) {
                if (is_target) {  /* a.b = ... */
                    /* reg. holding offset for field b is already pushed onto regstack, count it now. */
                    ++numregs_pushed;

                }
                else { /* ... = a.b */
                    
                    m1_reg offsetreg = popreg(comp->regstack);
                    m1_reg parentreg = popreg(comp->regstack); 
                    m1_reg reg       = gen_reg(comp, VAL_INT);
                
                    fprintf(OUT, "\tderef\t%c%d, %c%d, %c%d\n", reg_chars[(int)reg.type], reg.no,
                                                            reg_chars[(int)parentreg.type], parentreg.no,
                                                            reg_chars[(int)offsetreg.type], offsetreg.no);   
                    pushreg(comp->regstack, reg);
                    ++numregs_pushed;                                                           
                }
           // }
            
            /* set parent OUT parameter to the current node. */
            *parent = obj;
            
            break;
        }
        case OBJECT_DEREF: /* b in a->b */
        {
            m1_reg reg;
            gencode_obj(comp, obj->obj.field, parent, is_target);
        	reg = popreg(comp->regstack);
        	fprintf(OUT, "\tadd_i <struct>, I%d\n", reg.no);
            break;
        }
        case OBJECT_INDEX: /* b in a[b] */        
        {

            gencode_expr(comp, obj->obj.index);
                        
            if (is_target) { /* x[42] = ... */

                /* don't pop it, just count it as it was pushed onto regstack
                   by gencode_expr(). It will be popped by the calling function. 
                */
                ++numregs_pushed;
            
                
            }
            else { /* ... = x[42] */
                m1_reg offsetreg = popreg(comp->regstack); /* containing the index. */
                m1_reg parentreg = popreg(comp->regstack); /* containing the struct or array */
                m1_reg result    = gen_reg(comp, VAL_INT); /* to store the result. */ 
                
                fprintf(OUT, "\tderef\t%c%d, %c%d, %c%d\n", reg_chars[(int)result.type], result.no,
                                                            reg_chars[(int)parentreg.type], parentreg.no,
                                                            reg_chars[(int)offsetreg.type], offsetreg.no);   
                pushreg(comp->regstack, result);
                ++numregs_pushed;                                                            
            }
            
            /* set parent OUT parameter to current node. */
            *parent = obj;            
                    
            ++numregs_pushed;

            break;            
        }
        default:
            fprintf(stderr, "unknown object type in gencode_obj()\n");
            assert(0);
            break;
    }  
		
	/* return the number of registers that are pushed onto the stack in this function. */	
    return numregs_pushed;
		
}



static void
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
	
	gencode_expr(comp, w->cond);
	reg = popreg(comp->regstack);
	
	fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", startlabel, reg_chars[(int)reg.type], reg.no);
			
	/* remove break label from stack. */
	(void)pop(comp->breakstack);
}

static void
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
    
    gencode_expr(comp, w->cond);
    reg = popreg(comp->regstack);
    
    fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", startlabel, reg_chars[(int)reg.type], reg.no);

    fprintf(OUT, "L%d:\n", endlabel);
    
    (void)pop(comp->breakstack);

}

static void
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
        
    push(comp->breakstack, endlabel);
    
    if (i->init)
        gencode_expr(comp, i->init);

	fprintf(OUT, "L%d:\n", startlabel);
	
    if (i->cond) {
        m1_reg reg;
        gencode_expr(comp, i->cond);
        reg = popreg(comp->regstack);
        fprintf(OUT, "\tgoto_if L%d, %c%d\n", blocklabel, reg_chars[(int)reg.type], reg.no);
    }   

    fprintf(OUT, "\tgoto L%d\n", endlabel);
    
    fprintf(OUT, "L%d:\n", blocklabel);
    
    if (i->block) 
        gencode_expr(comp, i->block);
        
    if (i->step)
        gencode_expr(comp, i->step);
    
    fprintf(OUT, "\tgoto L%d\n", startlabel);
    fprintf(OUT, "L%d:\n", endlabel);
    
    (void)pop(comp->breakstack);
    
}

static void 
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

	
    gencode_expr(comp, i->cond);
    condreg = popreg(comp->regstack);
    
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
         
}

static void
gencode_deref(M1_compiler *comp, m1_object *o) {
    gencode_obj(comp, o, NULL, 0);   
}

static void
gencode_address(M1_compiler *comp, m1_object *o) {
    gencode_obj(comp, o, NULL, 0);       
}

static void
gencode_return(M1_compiler *comp, m1_expression *e) {
    gencode_expr(comp, e);
}

static void
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
	
	endlabel = gen_label(comp);
	
	/* generate code for left and get the register holding the result. */
	gencode_expr(comp, b->left);	
	left = popreg(comp->regstack);
	
	/* if left was not true, then need to evaluate right, otherwise short-cut. */
	fprintf(OUT, "\tgoto_if L%d, %c%d\n", endlabel, reg_chars[(int)left.type], left.no);
	
	/* generate code for right, and get the register holding the result. */
	gencode_expr(comp, b->right);	
	right = popreg(comp->regstack);
	
	/* copy the result from evaluating <right> into the reg. for left, and make it available on stack. */
	fprintf(OUT, "\tset\t%c%d, %c%d, x\n", reg_chars[(int)left.type], left.no, 
	                                       reg_chars[(int)right.type], right.no);
	pushreg(comp->regstack, left);
	
	fprintf(OUT, "L%d:\n", endlabel);
		
}

static void
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
	
	gencode_expr(comp, b->left);
	left = popreg(comp->regstack);
	
	/* if left was false, no need to evaluate right, and go to end. */
	fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", evalright, reg_chars[(int)left.type], left.no);		
	fprintf(OUT, "\tgoto L%d\n", endlabel);
	fprintf(OUT, "L%d:\n", evalright);
	
	gencode_expr(comp, b->right);
	right = popreg(comp->regstack);
	
	/* copy result from right to left result reg, as that's the reg that will be returned. */
	fprintf(OUT, "\tset\t%c%d, %c%d, x\n", reg_chars[(int)left.type], left.no, reg_chars[(int)right.type], right.no);	
	fprintf(OUT, "L%d:\n", endlabel);
	
	pushreg(comp->regstack, left);
}

/*

Helper function for != and == ops. The code generation template is the same,
except for one field. This is parameterized with the parameter is_eq_op, which
indicates whether it's the == op (is_eq_op = true) or the != op (is_eq_op = false).

*/
static void
ne_eq_common(M1_compiler *comp, m1_binexpr *b, int is_eq_op) {
    /* code for EQ; NE swaps the result.
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
    int endlabel, eq_ne_label;
    
    gencode_expr(comp, b->left);
    left = popreg(comp->regstack);
    
    gencode_expr(comp, b->right);
    right = popreg(comp->regstack);
    
    endlabel    = gen_label(comp);
    eq_ne_label = gen_label(comp);
    
    reg = gen_reg(comp, VAL_INT);
    fprintf(OUT, "\tsub_i\tI%d, %c%d, %c%d\n", reg.no, reg_chars[(int)left.type], left.no,
                                                      reg_chars[(int)right.type], right.no);
                                                      
    fprintf(OUT, "\tgoto_if L%d, %c%d\n", eq_ne_label, reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "\tset_imm\t%c%d, 0, %d\n", reg_chars[(int)reg.type], reg.no, is_eq_op);
    fprintf(OUT, "\tgoto L%d\n", endlabel);                                                      
    
    fprintf(OUT, "L%d:\n", eq_ne_label);
    fprintf(OUT, "\tset_imm\t%c%d, 0, %d\n", reg_chars[(int)reg.type], reg.no, !is_eq_op);
    fprintf(OUT, "L%d:\n", endlabel);
    
    pushreg(comp->regstack, reg);
}

static void
gencode_ne(M1_compiler *comp, m1_binexpr *b) {
    ne_eq_common(comp, b, 0);
}

static void
gencode_eq(M1_compiler *comp, m1_binexpr *b) {    
    ne_eq_common(comp, b, 1);  
}

static void
gencode_lt(M1_compiler *comp, m1_binexpr *b) {
    /* for LT (<) operator, use the ISGT opcode, but swap its arguments. */
    m1_reg result = gen_reg(comp, VAL_INT);
    m1_reg left, right;
    
    gencode_expr(comp, b->left);
    left = popreg(comp->regstack);
    
    gencode_expr(comp, b->right);
    right = popreg(comp->regstack);
    
    fprintf(OUT, "\tisgt_%c I%d, %c%d, %c%d\n", type_chars[(int)left.type], result.no, 
                                                reg_chars[(int)right.type], right.no,
                                                reg_chars[(int)left.type], left.no);

    pushreg(comp->regstack, result);
}

static void
gencode_le(M1_compiler *comp, m1_binexpr *b) {
    /* for LE (<=) operator, use the ISGE opcode, but swap its arguments. */
    m1_reg result = gen_reg(comp, VAL_INT);
    m1_reg left, right;
    
    gencode_expr(comp, b->left);
    left = popreg(comp->regstack);
    
    gencode_expr(comp, b->right);
    right = popreg(comp->regstack);
    
    fprintf(OUT, "\tisge_%c I%d, %c%d, %c%d\n", type_chars[(int)left.type], result.no, 
                                                reg_chars[(int)right.type], right.no,
                                                reg_chars[(int)left.type], left.no);

    pushreg(comp->regstack, result);
}


static void
gencode_binary(M1_compiler *comp, m1_binexpr *b) {
    char  *op = NULL;
    m1_reg left, 
    	   right,
    	   target;
    
    gencode_expr(comp, b->left);
    left = popreg(comp->regstack);

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
                op = "mult_i";
            else if (left.type == VAL_FLOAT)
                op = "mult_n";
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
            if (left.type == VAL_INT)
                op = "isgt_i";    
            else if (left.type == VAL_FLOAT)
                op = "isgt_n";
            else {
                fprintf(stderr, "wrong type for isgt");
                exit(EXIT_FAILURE);   
            }
            break;
        case OP_GE:
            if (left.type == VAL_INT)
                op = "isge_i";    
            else if (left.type == VAL_FLOAT)
                op = "isge_n";
            else {
                fprintf(stderr, "wrong type for isge");
                exit(EXIT_FAILURE);   
            }
            break;
        case OP_LT:
            gencode_lt(comp, b);
            return;
        case OP_LE:
            gencode_le(comp, b);
            return;
        case OP_EQ:
            gencode_eq(comp, b);
            return;
        case OP_NE: /* a != b;*/
            gencode_ne(comp, b);
            return;
        case OP_AND: /* a && b */
            gencode_and(comp, b);
            return;
        case OP_OR: /* a || b */
            gencode_or(comp, b);
            return;
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
    
    gencode_expr(comp, b->right);  
    right  = popreg(comp->regstack);
    
    target = gen_reg(comp, left.type);  
    
    fprintf(OUT, "\t%s\t%c%d, %c%d, %c%d\n", op, 
                                             reg_chars[(int)target.type], target.no, 
                                             reg_chars[(int)left.type], left.no, 
                                             reg_chars[(int)right.type], right.no);
    pushreg(comp->regstack, target);                                             

}


static void
gencode_not(M1_compiler *comp, m1_unexpr *u) {
    m1_reg reg, temp;
    int label1, label2;
    
    gencode_expr(comp, u->expr);
    reg = popreg(comp->regstack);
    
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
    
    pushreg(comp->regstack, temp);
   
}



static void
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
        case UNOP_NOT:
            return gencode_not(comp, u);
        default:
            op = "unknown op";
            break;   
    }   
    
    
    /* generate code for the pre/post ++ expression */ 
    gencode_expr(comp, u->expr);
    reg = popreg(comp->regstack);
    
    one    = gen_reg(comp, VAL_INT);
    target = gen_reg(comp, VAL_INT);
    
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n", one.no);
    fprintf(OUT, "\t%s\tI%d, I%d, I%d\n", op, target.no, reg.no, one.no);
    
    if (postfix == 0) { 
        /* prefix; return reg containing value before adding 1 */
    	reg.no = target.no; 
    	fprintf(OUT, "\tset\tI%d, I%d, x\n", reg.no, target.no);

    }	
    else {
        fprintf(OUT, "\tset\tI%d, I%d, x\n", reg.no, target.no);
    }
        
    pushreg(comp->regstack, reg);            
}

static void
gencode_break(M1_compiler *comp) {
	
	/* get label to jump to */
	int breaklabel = top(comp->breakstack);
	
	/* pop label from compiler
	's label stack (todo!) and jump there. */
    fprintf(OUT, "\tgoto\tL%d\n", breaklabel);
}

static void
gencode_funcall(M1_compiler *comp, m1_funcall *f) {
	m1_reg reg;
	m1_reg pmcreg;
	m1_reg offsetreg;
    m1_symbol *fun = sym_find_chunk(&comp->currentchunk->constants, f->name);
    
    if (fun == NULL) { /* XXX need to check in semcheck */
        fprintf(stderr, "Cant find function '%s'\n", f->name);
        ++comp->errors;
        return;
    }
    reg = gen_reg(comp, VAL_INT);
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", reg.no, fun->constindex);
    pmcreg = gen_reg(comp, VAL_CHUNK);
    offsetreg = gen_reg(comp, VAL_INT);
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", offsetreg.no, 0);
    fprintf(OUT, "\tderef\tP%d, CONSTS, I%d\n", pmcreg.no, reg.no);
    fprintf(OUT, "\tgoto_chunk\tP%d, I%d, x\n", pmcreg.no, offsetreg.no);
       

}


static void
gencode_print(M1_compiler *comp, m1_expression *expr) {

    m1_reg reg;
    m1_reg one;
    
    gencode_expr(comp, expr);
    reg = popreg(comp->regstack);
    fprintf(stderr, "[print] popped reg from stack %c%d\n", reg_chars[(int)reg.type], reg.no);
    
    one = gen_reg(comp, VAL_INT);
    
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n",  one.no);
	fprintf(OUT, "\tprint_%c\tI%d, %c%d, x\n", type_chars[(int)reg.type], one.no, 
	                                           reg_chars[(int)reg.type], reg.no);
		
}

static void
gencode_new(M1_compiler *comp, m1_newexpr *expr) {
	m1_reg reg     = gen_reg(comp, VAL_INT);
	m1_reg sizereg = gen_reg(comp, VAL_INT);
	

	unsigned size  = 128; /* fix; should be size of object requested */
    
    m1_decl *typedecl = type_find_def(comp, expr->type);
    
    if (typedecl == NULL) { /* XXX need to check in semcheck. */
        fprintf(stderr, "Cannot find type '%s' requested for in new-statement\n", expr->type);   
        
    }
    size = type_get_size(typedecl);
		
	fprintf(OUT, "\tset_imm I%d, 0, %d\n", sizereg.no, size);
	fprintf(OUT, "\tgc_alloc\tI%d, I%d, 0\n", reg.no, sizereg.no);
	
	pushreg(comp->regstack, reg);
}

static void
gencode_switch(M1_compiler *comp, m1_switch *expr) {
    /*
    switch (selector) {
        case val1:
            stat1
        case val2:
            stat2
        case val3:
            stat3
        default: 
            stat4  
    }
    
    translates to:
    
      sel = <evaluate selector>
    TEST1:
      sub_i result, selector, val1
      goto_if TEST2, result # if result is non-zero, then it's not case 1
      <code for stat1>
    TEST2:  
      sub_i result, selector, val2
      goto_if TEST3, result
      <code for stat2>
    TEST3:  
      sub_i result, selector, val3
      goto_if TEST4, result
      <code for stat3>
    TEST4:
      <code for default>
    END: #break statements will go here.
      
    */
    m1_reg reg;
    int endlabel = gen_label(comp);

    m1_case *caseiter;
    m1_reg test;
    test = gen_reg(comp, VAL_INT);
    
    /* evaluate selector */
    gencode_expr(comp, expr->selector);
    reg = popreg(comp->regstack);
    
    push(comp->breakstack, endlabel); /* for break statements to jump to. */    


    caseiter = expr->cases;
    
    while (caseiter != NULL) {
        int testlabel;
        
        fprintf(OUT, "\tsub_i\tI%d, I%d, I%d\n", test.no, reg.no, caseiter->selector);
     
        testlabel = gen_label(comp);
        fprintf(OUT, "\tgoto_if L%d, I%d\n", testlabel, test.no);

        gencode_block(comp, caseiter->block);
        
        fprintf(OUT, "L%d:\n", testlabel);

        caseiter = caseiter->next;   
    }
    
    if (expr->defaultstat) {
       gencode_block(comp, expr->defaultstat); 
    }
    
    fprintf(OUT, "L%d:\n", endlabel);      
    (void)pop(comp->breakstack);
    
}

static void
gencode_var(M1_compiler *comp, m1_var *v) {    
    if (v->init) { /* generate code only for initializations. */
       m1_reg     reg;
       m1_symbol *sym;
       
       /* generate code for initialisation) */
       gencode_expr(comp, v->init);     
       reg = popreg(comp->regstack);
              
       assert(v->sym != NULL);
       sym = v->sym;       
       
       /* check for first usage of this variable; may not have a reg allocated yet. */       
       if (sym->regno == NO_REG_ALLOCATED_YET) {
            sym->regno = reg.no;
       }
    }
    
    if (v->size > 1) { /* generate code to allocate memory on the heap for arrays */
        m1_symbol *sym;
        m1_reg     memsize;        
        
        int size_per_item = 4; /* XXX fix this. Size of one element in the array. */
        int size;

        sym = v->sym;
        assert(sym != NULL);
        
        if (sym->regno == NO_REG_ALLOCATED_YET) {
            m1_reg reg = gen_reg(comp, sym->valtype);
            sym->regno = reg.no;
        }
        
        /* calculate total size of array. If smaller than 256*255,
         * then load the value with set_imm, otherwise from the 
         * constants segment.
         */
        size = v->size * size_per_item;
        
        memsize = gen_reg(comp, VAL_INT); /* reg to hold num of bytes to allocate */
        
        if (size < (256*255)) {
            int remainder = size % 256;   
            int num256    = (size - remainder) / 256;
            fprintf(OUT, "\tset_imm\tI%d, %d, %d\n", memsize.no, num256, remainder);
        }
        else {
            m1_symbol *sizesym = sym_find_int(&comp->currentchunk->constants, size);
            assert(sizesym != NULL);
            fprintf(OUT, "\tderef\tI%d, CONSTS, %d\n", memsize.no, sizesym->constindex);
        }
        
        fprintf(OUT, "\tgc_alloc\tI%d, I%d, 0\n", sym->regno, memsize.no);
    }
       
}



static void
gencode_vardecl(M1_compiler *comp, m1_var *v) {
    /* There may be a list of m1_vars. */
    m1_var *iter = v;
    while (iter != NULL) {
        gencode_var(comp, iter);
        iter = iter->next;   
    }   
}

static void
gencode_cast(M1_compiler *comp, m1_castexpr *expr) {
    m1_reg reg;
    m1_reg result;
    
    gencode_expr(comp, expr->expr);
    reg = popreg(comp->regstack);
    
    /* XXX do these type checks in semcheck. */
    if (strcmp(expr->type, "int") == 0) {
        result = gen_reg(comp, VAL_INT);
        fprintf(OUT, "\tntoi\tI%d, %c%d, x\n", result.no, reg_chars[(int)reg.type], reg.no);
    }
    else if (strcmp(expr->type, "num") == 0) {
        result = gen_reg(comp, VAL_FLOAT);
        fprintf(OUT, "\titon\tN%d, %c%d, x\n", result.no, reg_chars[(int)reg.type], reg.no);
    }
    else {
        fprintf(stderr, "wrong type in casting\n");
        exit(EXIT_FAILURE);   
    }
    
    pushreg(comp->regstack, result);
  
}

static void
gencode_expr(M1_compiler *comp, m1_expression *e) {
            
    if (e == NULL) {
    	debug("expr e is null in gencode_expr\n");
    }
        
    switch (e->type) {
        case EXPR_NUMBER:
            gencode_number(comp, e->expr.l);
            break;
        case EXPR_INT:
            gencode_int(comp, e->expr.l);
            break;
        case EXPR_STRING:
            gencode_string(comp, e->expr.l);     
            break;
        case EXPR_BINARY:
            gencode_binary(comp, e->expr.b);
            break;
        case EXPR_UNARY:
            gencode_unary(comp, e->expr.u);
            break;
        case EXPR_FUNCALL:
            gencode_funcall(comp, e->expr.f);
            break;
        case EXPR_ASSIGN:
            gencode_assign(comp, e->expr.a);
            break;
        case EXPR_IF:   
            gencode_if(comp, e->expr.i);
            break;
        case EXPR_WHILE:
            gencode_while(comp, e->expr.w);
            break;
        case EXPR_DOWHILE:
            gencode_dowhile(comp, e->expr.w);
            break;
        case EXPR_FOR:
            gencode_for(comp, e->expr.o);
            break;
        case EXPR_RETURN:
            gencode_return(comp, e->expr.e);
            break;
        case EXPR_NULL:
            gencode_null(comp);
            break;
        case EXPR_DEREF:
            gencode_deref(comp, e->expr.t);
            break;
        case EXPR_ADDRESS:
            gencode_address(comp, e->expr.t);
            break;
        case EXPR_OBJECT: 
        {
            m1_object *obj; /* temp. storage. */
            gencode_obj(comp, e->expr.t, &obj, 0);            
            break;
        }
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
            gencode_switch(comp, e->expr.s);
        	break;
        case EXPR_NEW:
        	gencode_new(comp, e->expr.n);
        	break;
        case EXPR_PRINT:
            gencode_print(comp, e->expr.e);   
            break; 
        case EXPR_CAST:
            gencode_cast(comp, e->expr.cast);
            break;
        default:
            fprintf(stderr, "unknown expr type (%d)", e->type);   
            exit(EXIT_FAILURE);
    }   
}


/*

Generate the constants segment.

*/
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

/*

Generate the metadata segment. 

*/
static void
gencode_metadata(m1_chunk *c) {
    assert(c != NULL);
	fprintf(OUT, ".metadata\n");	
}


static void
gencode_block(M1_compiler *comp, m1_expression *block) {
    m1_expression *iter = block;
    
    while (iter != NULL) {
        gencode_expr(comp, iter);
        iter = iter->next;
    }
}

static void 
gencode_chunk(M1_compiler *comp, m1_chunk *c) {
    
    int i;
   
#define PRELOAD_0_AND_1     0
    
#if PRELOAD_0_AND_1    
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
        /* set pointer to current chunk, so that the code generator 
           has access to anything that belongs to the chunk. 
         */
        comp->currentchunk = iter;   
        gencode_chunk(comp, iter);
        iter = iter->next;   
    }
}



