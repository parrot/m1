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
#include "instr.h"

#include "ann.h"

#define OUT	stdout


#define M1DEBUG 1

#ifdef M1DEBUG
    #define debug(x)    fprintf(stderr, x);
#else
    #define debug(x)
#endif

/* See PDD32 for these constants. */
#define M0_REG_I0   12
#define M0_REG_N0   73       
#define M0_REG_S0   134
#define M0_REG_P0   195

static unsigned gencode_expr(M1_compiler *comp, m1_expression *e);
static void gencode_block(M1_compiler *comp, m1_block *block);
static unsigned gencode_obj(M1_compiler *comp, m1_object *obj, m1_object **parent, unsigned *dimension, int is_target);

static const char type_chars[REG_TYPE_NUM] = {'i', 'n', 's', 'p'};
static const char reg_chars[REG_TYPE_NUM] = {'I', 'N', 'S', 'P'};

/*
#define M1_GEN_INS(name, cf, ops, pc) { \
      if (M1_DEBUG) { \
        fprintf(stderr, "pc = %d, op: " #name "\n", (char)pc);      \
      } \
      m1_ins_##name( cf, &ops[4*pc] ); \
  }
*/


static void
reset_reg(M1_compiler *comp) {
    /* Set all fields in the registers table to 0. */
    memset(comp->registers, 0, sizeof(char) * REG_NUM * REG_TYPE_NUM);    
}

#define REG_UNUSED  0
#define REG_USED    1
#define REG_SYMBOL  2

static m1_reg
alloc_reg(M1_compiler *comp, m1_valuetype type) {
    m1_reg r;
    int i = 0;
    /* look for first empty slot. */
    while (i < REG_NUM && comp->registers[type][i] != REG_UNUSED) {
        i++;
    }
    
    /* XXX Need to properly handle spilling when out of registers. */
    if (i >= REG_NUM) {
        fprintf(stderr, "Out of registers!! Resetting it, hoping for the best!\n");
        memset(comp->registers[type], 0, sizeof(char) * REG_NUM);
    }
    
    
    /* set the newly allocated register to "used". */
    comp->registers[type][i] = REG_USED;
    
    /* return the register. */
    r.no        = i;    
    r.type      = type;
    return r;
}

/*

Throughout the code, we call free_reg() on registers that we think we 
no longer need. Sometimes, these registers have been assigned to a symbol. 
Symbols get to keep what they get. In order to prevent very difficult code, 
just note that the register is used by a symbol by "freezing" it. 
When free_reg() is called on it (after it's frozen), it won't be freed by 
free_reg().

*/
static void
freeze_reg(M1_compiler *comp, m1_reg r) {
    assert(comp != NULL);
    assert(r.type < REG_TYPE_NUM);
    assert(r.no < REG_NUM);
    assert(r.no >= 0);
    
    comp->registers[r.type][r.no] = REG_SYMBOL;   
}

/*

Make register C<r> available again, unless it's assigned to a symbol.
In that case, the register is left alone. 

*/
static void
free_reg(M1_compiler *comp, m1_reg r) {
    
    /* if m1 was invoked with -r, then switch off free_reg(). */
    if (comp->no_reg_opt)
        return;
    
    /* if it's not frozen, it may be freed. */
    if (comp->registers[r.type][r.no] != REG_SYMBOL) {
//        fprintf(stderr, "Unusing %d for good\n", r.no);        
        comp->registers[r.type][r.no] = REG_UNUSED;
    }
    
    /* XXX this is for debugging. */  
    /*
    int i;
    for (i = 0; i < REG_NUM; i++)
        fprintf(stderr, "%d", i % 10);
    
    fprintf(stderr, "\n");    
    for (i = 0; i < REG_NUM; i++) {
        fprintf(stderr, "%d", comp->registers[r.type][i]);   
    }
    fprintf(stderr, "\n\n");
    */
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
    assert(lit->type == VAL_FLOAT);
    assert(lit->sym != NULL);
       
    reg        = alloc_reg(comp, VAL_FLOAT);
    constindex = alloc_reg(comp, VAL_INT);
        
    //ins_set_imm(comp, &constindex, 0, lit->sym->constindex);
    //ins_deref(comp, &reg, CONSTS, &constindex);
    
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", constindex.no, lit->sym->constindex);
    fprintf(OUT, "\tderef\tN%d, CONSTS, I%d\n", reg.no, constindex.no);

    free_reg(comp, constindex);
    pushreg(comp->regstack, reg);
    
} 

static void
gencode_char(M1_compiler *comp, m1_literal *lit) {
	/*
	deref Nx, CONSTS, <const_id>
	*/
    m1_reg reg;
    
    assert(comp != NULL);
    assert(lit != NULL);
    assert(lit->type == VAL_INT);
    assert(lit->sym != NULL);
       
    reg = alloc_reg(comp, VAL_INT);
        
    /* reuse the reg, first for the index, then for the result. */        
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", reg.no, lit->sym->constindex);
    fprintf(OUT, "\tderef\tI%d, CONSTS, I%d\n", reg.no, reg.no);
        
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

    reg = alloc_reg(comp, VAL_INT);
      
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
        /* XXX this will fail if constindex > 255. */
        fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", reg.no, lit->sym->constindex);
        fprintf(OUT, "\tderef\tI%d, CONSTS, I%d\n", reg.no, reg.no);

    }
    
    pushreg(comp->regstack, reg);
    
}

static void
gencode_null(M1_compiler *comp) {
    m1_reg reg;
    reg = alloc_reg(comp, VAL_INT);
	/* "null" is just 0, but then in a "pointer" context. */
    fprintf(OUT, "\tset_imm\tI%d, 0, 0\n", reg.no);
    
    pushreg(comp->regstack, reg);
}   


static void
gencode_bool(M1_compiler *comp, int boolval) {
    /* Generate one of these:
       set_imm Ix, 0, 1 # for true
       set_imm Ix, 0, 0 # for false
    */
    m1_reg reg = alloc_reg(comp, VAL_INT);
    fprintf(OUT, "\tset_imm\t%d, 0, %d\n", reg.no, boolval);
    pushreg(comp->regstack, reg);   
}

static void
gencode_string(M1_compiler *comp, m1_literal *lit) {
    m1_reg stringreg,         
           constidxreg;
    
    assert(comp != NULL);
    assert(lit != NULL);
    assert(lit->sym != NULL);
    assert(lit->type == VAL_STRING);
    
    stringreg   = alloc_reg(comp, VAL_STRING);
    constidxreg = alloc_reg(comp, VAL_INT);
      
    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", constidxreg.no, lit->sym->constindex);
    fprintf(OUT, "\tderef\tS%d, CONSTS, I%d\n", stringreg.no, constidxreg.no);
       
    free_reg(comp, constidxreg);
    
    pushreg(comp->regstack, stringreg);
}

/* Generate code for assignments. */
static void
gencode_assign(M1_compiler *comp, NOTNULL(m1_assignment *a)) {
    m1_object *parent_dummy;  /* pointer storage needed for code generation of LHS. */
    unsigned   lhs_reg_count; /* number of regs holding result of LHS (can be aggregate/indexed) */
	unsigned   rhs_reg_count; /* number of regs holding result of RHS (can be aggregate/indexed) */
    unsigned   dimension_dummy = 0;
    		
    assert(a != NULL);
	
	/* generate code for RHS and get number of registers that hold the result */
    rhs_reg_count = gencode_expr(comp, a->rhs);
    
    /* Generate code for LHS and get number of registers that hold the result 
       Note the "1" argument; this is to indicate we want to generate code for LHS
       as an l-value.
     */
    lhs_reg_count = gencode_obj(comp, a->lhs, &parent_dummy, &dimension_dummy, 1);    
    

    if (rhs_reg_count == 2) { /* deref; ... = x[42] */
        if (lhs_reg_count == 1) { /* lhs_reg_count + rhs_reg_count == 3, so pop 3 regs. */
        
            m1_reg target = popreg(comp->regstack);
            m1_reg index  = popreg(comp->regstack);
            m1_reg parent = popreg(comp->regstack);

            fprintf(OUT, "\tderef\t%c%d, %c%d, %c%d\n", reg_chars[(int)target.type], target.no, 
                                                        reg_chars[(int)parent.type], parent.no,
                                                        reg_chars[(int)index.type], index.no);
        }
        else {
            assert(0); /* should never happen. */          
        }
    }
    else { /* must be set_ref or set */
    
        if (lhs_reg_count == 1) { /* just a simple lvalue; a = b; */    
            m1_reg lhs = popreg(comp->regstack);    
            m1_reg rhs = popreg(comp->regstack);
            fprintf(OUT, "\tset \t%c%d, %c%d, x\n", reg_chars[(int)lhs.type], lhs.no, 
                                                    reg_chars[(int)rhs.type], rhs.no);
            pushreg(comp->regstack, rhs);
        }
        else if (lhs_reg_count == 2) { /* complex lvalue; x[10] = 42 */
            assert(rhs_reg_count == 1); /* lhs_reg_count+rhs_reg_count == 3, so pop 3 regs. */
            
            m1_reg index  = popreg(comp->regstack);
            m1_reg parent = popreg(comp->regstack);
            m1_reg rhs    = popreg(comp->regstack);
        
            fprintf(OUT, "\tset_ref\t%c%d, %c%d, %c%d\n", reg_chars[(int)parent.type], parent.no, 
                                                          reg_chars[(int)index.type], index.no,
                                                          reg_chars[(int)rhs.type], rhs.no);
            free_reg(comp, index);                                                      
            free_reg(comp, parent);
            
            /* make result available for next in "chain" of assignments, if any (e.g, a = b = c = 42;). */              
            pushreg(comp->regstack, rhs);
        }
    }    
}

/*

Generate instructions for an m1_object node; this may be as simple as a single identifier
(e.g., "x"), or as complex as a combination of array access and struct member access 
(e.g., x[1][2][3], x.y.z). The function returns the number of registers that are used
to refer to the object; this is either 1 (simple case) or 2 (array or struct). Though
an object can be of arbitrary complexity, this is always reduced to 2 registers
(possibly emitting instructions in this function; as soon as 3 registers are needed,
an instruction is emitted, and one register is removed. See the comments inside.)

The dimension parameter is also an OUT parameter, and is used in particular for arrays.
When handling x[1][2][3], and we're calculating the offset from the base address (x),
we need to know which dimension we're in (1, 2 or 3 in the example of x[1][2][3]).

*/
static unsigned
gencode_obj(M1_compiler *comp, m1_object *obj, m1_object **parent, unsigned *dimension, int is_target) 
{

    unsigned numregs_pushed = 0;

    assert(comp != NULL);
    assert(comp->currentchunk != NULL);
    assert(comp->currentsymtab != NULL);
	   
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
  	             
    As an example of multi-dimensional arrays:
    
    x[1][2][3] looks like this:
    
        OBJECT_MAIN
                | OBJECT_INDEX (3x)
                |    |   |   |
                V    V   V   V
                 x   1   2   3
                  \ /   /   /
OBJECT_LINK------> L1  /   /
                    \ /   /
OBJECT_LINK------>   L2  /
                      \ / 
OBJECT_LINK------>     L3
                       ^
                       |
                      ROOT
    
    Case for l-values: (x[1][2] = ...)
    ==================================
    When traversing the tree starting at ROOT (L3), we go down towards OBJECT_MAIN
    first by following the nodes' <parent> links. From L3->L2->L1->x. Then the OUT
    parameter <parent> is set to x, and the register containing x (say, I0) is 
    pushed onto the regstack. We go back to L1, as x's parent is NULL. Then 
    L1's field will be visited, which is [3]. We load [1] into a register, and push 
    it onto the regstack. Then we go back to L2. We visit its <field>, which is
    [2]. We load [2] into a register and push that register onto the regstack. 
    The regstack now looks like (growing bottom-up) this:
    
    |        |
    | I2 (2) | <--SP (stack pointer)
    | I1 (1) |
    | I0 (x) |
    ---------- <--SB (stack base)
    
    Since set_ref and deref ops only use 2 operands for the target or source, respectively,
    we need to combine 2 registers into 1. We'll combine I0 and I1, by adding I1 to I0.
    Access to an array element x[1][2] in an array defined as "int x[4][3]" is laid out 
    as follows, marked with "XX" : (|--| is 1 integer)
    
           x[0][0]         x[1][2] 
             |        x[1]    |
             |         |      |
             V         |      V
                       V      XX 
            |--|--|--| |--|--|--| |--|--|--| |--|--|--| 
              0  1  2    0  1  2    0  1  2    0  1  2             [3]
            __________ __________ __________ __________
                 0          1          2          3                x[4]
    
    Therefore, x[1] represents the base address of "x" + "1 X 3" (since each "element"
    in the first dimension has length 3). Therefore, generate the following instruction:
    
    set_imm  I4,  0,  3   # length of each "element" in first dimension.
    mult_i   I1, I1, I4   # multiply [1] by 3
    add_i    I0, I0, I1   # x = x + 3.
    
    For this, we can pop off the 3 register so far, and push back I0 and I2, resulting in:
    
    
    |            |
    | I2 (4)     |  <---SP
    | I0 (x+[3]) |
    --------------  <---SB 
    
    For each additional dimension, do the same thing, whenever there are 3 registers 
    on the stack, reduce it to two.
    
    Case for r-values: ( ... = x[1][2])
    ===================================
    Same example, accessing x[1][2] in same array defined as x[4][3], as above.
    
    Instead of "set_ref", we want to generate "deref".
    
    I0 contains pointer to "x".
    Load 1 and 2 in registers I1 and I2 respectively, so the stack looks like this:
    
    |    |
    | I2 | # holds 2
    | I1 | # holds 1
    | I0 | # holds x
    ------
    
    Since we want to "dereference" x with indices [1] and [2], storing the result in [3]
    we want to write:
    
    deref I3, I0, I1, I2
    
    but since there are only 3 operands, I0 and I1 are combined into I0:
    
    deref I3, I0, I2
    
    For that to work, I0 and I1 need to be combined:
    
    set_imm   I4, 0, 3    # load size of elements in first dimension
    mult_i    I1, I1, I4  # multiply I1 by 3
    add_i     I0, I0, I1  # x = x + 3
    
    and then the deref instruction.
        
                        
    */

    switch (obj->type) {
        case OBJECT_LINK:
        {
            /* set OUT parameter to this node (that's currently visited, obj) */
            *parent = obj;   	

            /* count the number of regs pushed by the parent, which is 1. */
            numregs_pushed += gencode_obj(comp, obj->parent, parent, dimension, is_target);
            
            /* At this point, we're done visiting parents, so now visit the "fields".
               In x.y.z, after returning from x, we're visiting y. After that, we'll visit z.
               As we do this, keep track of how many registers were used to store the result.
             */
            
            numregs_pushed += gencode_obj(comp, obj->obj.field, parent, dimension, is_target);   
                                   
            if (numregs_pushed == 3) {
                /* if the field was an index. (a[b]) */
                if (obj->obj.field->type == OBJECT_INDEX) {
                    m1_reg last           = popreg(comp->regstack);   /* latest added; store here for now. */
                    m1_reg field          = popreg(comp->regstack);   /* 2nd latest, this one needs to be removed. */
                    m1_reg parentreg      = popreg(comp->regstack);   /* x in x[2][3]. */                
                    m1_reg size_reg       = alloc_reg(comp, VAL_INT); /* to hold amount to add. */
                    m1_reg updated_parent = alloc_reg(comp, VAL_INT); /* need to copy base address from parent. */
                       
                    fprintf(stderr, "DIMENSION: %d\n", *dimension);                                
                    /* 3 registers only the case when 2 dimensions are parsed, e.g., x[10][20].
                       Find the size of the first dimension, since that's the one that's 
                       added to the parent's base address, i.e., adding 10 * sizeof(type).

                       Get a pointer to the m1_var node for the parent; this is accessible
                       through the <sym> field of the parent; m1_var and m1_symbol nodes
                       have pointers to each other.
                     */
                    fprintf(stderr, "Parent: %s\n", (*parent)->obj.name);
                    assert((*parent)->sym != NULL);
                    assert((*parent)->sym->var != NULL);
                    m1_var       *parent_var        = (*parent)->sym->var; 
                    m1_dimension *current_dimension = parent_var->dims;

                    /* now find right dimension. The number in dimension keeps track of
                       which dimension we're currently visiting. Dimensions are stored in a
                       linked list, so set the pointer <num_get_next> times to the dimension 
                       node's next.
                     */
                    { /* local scope to limit num_get_text. */
                        unsigned num_get_next;
                        for (num_get_next = *dimension - 1; num_get_next != 0; num_get_next--) {
                            assert(current_dimension->next != NULL);
                            current_dimension = current_dimension->next;
                        }
                    }
                     
                    fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", size_reg.no, current_dimension->num_elems);
                    fprintf(OUT, "\tmult_i\tI%d, I%d, I%d\n", field.no, field.no, size_reg.no);
                    /* Need to have the following instruction (set X, Y), otherwise it doesn't work. */
                    fprintf(OUT, "\tset \tI%d, I%d, x\n", updated_parent.no, parentreg.no); 
                    fprintf(OUT, "\tadd_i\tI%d, I%d, I%d\n", updated_parent.no, updated_parent.no, field.no);
                
                    pushreg(comp->regstack, updated_parent);   /* push back (x+[2]) */
                    pushreg(comp->regstack, last);             /* push back the latest added one. */
                    
                    free_reg(comp, size_reg);
                    free_reg(comp, field);
                    /* we popped 3, and pushed 2, so effectively decrement by 1. */
                    --numregs_pushed;
                }              
                else if (obj->obj.field->type == OBJECT_FIELD) {
                    /* field is a struct member access (a.b) */
                    m1_reg last      = popreg(comp->regstack);
                    m1_reg offset    = popreg(comp->regstack);
                    m1_reg parentreg = popreg(comp->regstack);
                    m1_reg target = alloc_reg(comp, VAL_INT);
                    
                    fprintf(OUT, "\tderef\tI%d, I%d, I%d\n", target.no, parentreg.no, offset.no);   
                    
                    pushreg(comp->regstack, target);
                    pushreg(comp->regstack, last);
                    /* popped 3 regs; pushed 2, so decrement numregs_pushed. */
                    free_reg(comp, offset);
                    --numregs_pushed;
                                        
                }
            }
            break;
            
        }
        case OBJECT_MAIN: 
        {   
            m1_reg reg;              
            fprintf(stderr, "OBJECT MAIN: %s\n", obj->obj.name);
        	assert(obj->obj.field != NULL);
        	assert(obj->sym != NULL);
        	assert(obj->sym->typedecl != NULL);
 
        	/* if symbol has not register allocated yet, do it now. */
        	if (obj->sym->regno == NO_REG_ALLOCATED_YET) {
                m1_reg r        = alloc_reg(comp, obj->sym->typedecl->valtype);
                obj->sym->regno = r.no;
                freeze_reg(comp, r);                
        	}  
            
            /* get the storage type. */
            if (obj->sym->num_elems > 1) { /* it's an array! store it in an int register. */
                reg.type = VAL_INT;                
            }
            else { 
             /* it's not an array; just get the root type (in string[10], that's string). */
                assert(obj->sym->num_elems == 1);                
                reg.type = obj->sym->typedecl->valtype;     
            }
            
        	reg.no = obj->sym->regno;    
        	freeze_reg(comp, reg);   	
                      
            /* return a pointer to this node by OUT parameter. */
            *parent = obj;

            pushreg(comp->regstack, reg);            
            ++numregs_pushed; /* just pushed, count it. */
            
            break;
        }
        case OBJECT_FIELD: /* example: b in a.b */
        {            
            m1_reg fieldreg = alloc_reg(comp, VAL_INT);/* reg for storing offset of field. */            
            
            assert((*parent) != NULL);                                 
            assert((*parent)->sym != NULL);
            assert((*parent)->sym->typedecl != NULL);
                        
            /* parent's symbol has a typedecl node, which holds the structdef (d.s), which has a symbol table. */
            m1_symbol *fieldsym = sym_lookup_symbol(&(*parent)->sym->typedecl->d.s->sfields, obj->obj.name);
                       
            assert(fieldsym != NULL);
            
            /* load the offset into a reg. and make it available through the regstack. */
            fprintf(OUT, "\tset_imm  I%d, 0, %d\n", fieldreg.no, fieldsym->offset);             

            /* make it available through the regstack */
            pushreg(comp->regstack, fieldreg);
            ++numregs_pushed;                                

            /* set parent OUT parameter to the current node. */
            *parent = obj;
            
            /* whenever there's a field, reset dimension;
               e.g. in x[4].y[5], when handling [5], dimension should be 1 again, 
               not 2. Since the "chain" is broken by the field y in the middle,
               dimension needs to be reset.
            */ 
            (*dimension) = 0;
            
            break;
        }
        case OBJECT_DEREF: /* b in a->b */
        {
            fprintf(stderr, "a->b is not yet implemented\n");
            assert(0);
            break;
        }
        case OBJECT_INDEX: /* b in a[b] */        
        {
            (*dimension)++; /* increment dimension whenever we handle an index. */
            numregs_pushed += gencode_expr(comp, obj->obj.index);                                                              
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
	   goto LTEST
	LBLOCK
	  <block>	
	LTEST:
	   code for <cond>
	   goto_if <cond>, LBLOCK
	   ...	
	*/
	m1_reg reg;
	int startlabel = gen_label(comp), 
	    endlabel   = gen_label(comp);
	
	/* push break label onto stack so break statement knows where to go. */
	push(comp->breakstack, endlabel);
	push(comp->continuestack, startlabel);
	
	fprintf(OUT, "\tgoto L%d\n", endlabel);
	
	fprintf(OUT, "L%d:\n", startlabel);
	gencode_expr(comp, w->block);
	
	fprintf(OUT, "L%d:\n", endlabel);
	
	gencode_expr(comp, w->cond);
	reg = popreg(comp->regstack);
	
	fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", startlabel, reg_chars[(int)reg.type], reg.no);
	
	free_reg(comp, reg);
			
	/* remove break and continue labels from stack. */
	(void)pop(comp->breakstack);
	(void)pop(comp->continuestack);
}

static void
gencode_dowhile(M1_compiler *comp, m1_whileexpr *w) {
	/*
	
	LSTART:
	  <code for block>
	  cond = <code for cond>
	  goto_if LSTART, cond
	  
	*/
    m1_reg reg;
    
    int startlabel = gen_label(comp);
    int endlabel   = gen_label(comp);
    
    push(comp->breakstack, endlabel);
    push(comp->continuestack, startlabel);
     
    fprintf(OUT, "L%d:\n", startlabel);
    gencode_expr(comp, w->block);
    
    gencode_expr(comp, w->cond);
    reg = popreg(comp->regstack);
    
    fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", startlabel, reg_chars[(int)reg.type], reg.no);

    fprintf(OUT, "L%d:\n", endlabel);
    
    free_reg(comp, reg);
    
    (void)pop(comp->breakstack);
    (void)pop(comp->continuestack);

}

static void
gencode_for(M1_compiler *comp, m1_forexpr *i) {
	/*		
      <code for init>
	LSTART:
	  <code for cond>
	  goto_if cond, LBLOCK
      goto LEND
    LBLOCK: 
      <code for block>
      <code for step>
	  goto LSTART
	LEND:
	
	*/
    int startlabel = gen_label(comp), 
        endlabel   = gen_label(comp),
        steplabel  = gen_label(comp), 
        blocklabel = gen_label(comp); /* label where the block starts */
        
    push(comp->breakstack, endlabel);
    push(comp->continuestack, steplabel); /* XXX check if this is the right label. */
    
    if (i->init)
        gencode_expr(comp, i->init);

	fprintf(OUT, "L%d:\n", startlabel);
	
    if (i->cond) {
        m1_reg reg;
        gencode_expr(comp, i->cond);
        reg = popreg(comp->regstack);
        fprintf(OUT, "\tgoto_if L%d, %c%d\n", blocklabel, reg_chars[(int)reg.type], reg.no);

        free_reg(comp, reg);
    }   

    fprintf(OUT, "\tgoto L%d\n", endlabel);
    
    fprintf(OUT, "L%d:\n", blocklabel);
    
    if (i->block) 
        gencode_expr(comp, i->block);
        
    fprintf(OUT, "L%d:\n", steplabel);
    if (i->step)
        gencode_expr(comp, i->step);
    
    fprintf(OUT, "\tgoto L%d\n", startlabel);
    fprintf(OUT, "L%d:\n", endlabel);
    
    (void)pop(comp->breakstack);
    (void)pop(comp->continuestack);
    
}

static void 
gencode_if(M1_compiler *comp, m1_ifexpr *i) {
	/*	
      result = <evaluate condition>
	  goto_if LIF, result
	  <code for elseblock>
	  goto LEND
    LIF:
	  <code for ifblock>
	LEND:
	
	*/
    m1_reg condreg;
    int endlabel = gen_label(comp),
        iflabel  = gen_label(comp);

	
    gencode_expr(comp, i->cond);

    condreg = popreg(comp->regstack);

    fprintf(OUT, "\tgoto_if\tL%d, %c%d\n", iflabel, reg_chars[(int)condreg.type], condreg.no);

    free_reg(comp, condreg);
    
    /* else block */
    if (i->elseblock) {            	
        gencode_expr(comp, i->elseblock);     
    }
    fprintf(OUT, "\tgoto L%d\n", endlabel);
    
    /* if block */
    fprintf(OUT, "L%d:\n", iflabel);
    gencode_expr(comp, i->ifblock);
			
    fprintf(OUT, "L%d:\n", endlabel);
         
}

static void
gencode_deref(M1_compiler *comp, m1_object *o) {
    unsigned dimension_dummy = 0;
    /* XXX need equivalent of C's *obj operator. */
    gencode_obj(comp, o, NULL, &dimension_dummy, 0);   
}

static void
gencode_address(M1_compiler *comp, m1_object *o) {
    unsigned dimension_dummy = 0;
    /* XXX need equivalent of C's &obj operator. */
    gencode_obj(comp, o, NULL, &dimension_dummy, 0);       
}

static void
gencode_return(M1_compiler *comp, m1_expression *e) {
        
    m1_reg chunk_index,
           retpc_reg,
           retpc_index;
    
    if (e != NULL) {
        /* returning a value:
          
           <code for expr> # result is stored in RY.
           
           set_imm IX, 0, R0   # get the number of index R0 and store in IX
           set_ref CF, IX, RY  # store value in RY in CF[IX].
        */
        gencode_expr(comp, e);

        m1_reg retvalreg = popreg(comp->regstack);
        m1_reg indexreg  = alloc_reg(comp, VAL_INT);
        
        /* load the number of register R0 */
        fprintf(OUT, "\tset_imm\tI%d, 0, %c0\n", indexreg.no, reg_chars[(int)retvalreg.type]);
        /* index the current callframe, and set in its R0 register the value from the return expression. */
        fprintf(OUT, "\tset_ref\tCF, I%d, %c%d\n", indexreg.no, reg_chars[(int)retvalreg.type], retvalreg.no);

        free_reg(comp, indexreg);
        free_reg(comp, retvalreg);

        /*  make register available. XXX is this needed? */

    }

    /* instructions to return:
     
       set_imm    IX, 0, RETPC
       deref      IY, PCF, IX
       set_imm    IZ, 0, CHUNK
       deref      IZ, PCF, IZ
       goto_chunk IZ, IY
    */

    chunk_index = alloc_reg(comp, VAL_INT);
    retpc_reg   = alloc_reg(comp, VAL_INT);
    retpc_index = alloc_reg(comp, VAL_INT);

    fprintf(OUT, "\tset_imm    I%d, 0, RETPC\n", retpc_index.no);
    fprintf(OUT, "\tderef      I%d, PCF, I%d\n", retpc_reg.no, retpc_index.no);
    fprintf(OUT, "\tset_imm    I%d, 0, CHUNK\n", chunk_index.no);
    fprintf(OUT, "\tderef      I%d, PCF, I%d\n", chunk_index.no, chunk_index.no);
    fprintf(OUT, "\tgoto_chunk I%d, I%d, x\n", chunk_index.no, retpc_reg.no);        
    free_reg(comp, chunk_index);    
    free_reg(comp, retpc_reg);
    free_reg(comp, retpc_index);

}

static void
gencode_or(M1_compiler *comp, m1_binexpr *b) {
	/*
	  a || b
	  
	  left = <evaluate a>
	  goto_if LEND, left
	  right = <evaluate b>
	  left = right 
	LEND:
	
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
	free_reg(comp, right);
	
	fprintf(OUT, "L%d:\n", endlabel);
		
}

static void
gencode_and(M1_compiler *comp, m1_binexpr *b) {
	/*
	  a && b
	  
	  left = <evaluate a>
	  goto_if LRIGHT, left, 
	  goto LEND
	LRIGHT:
	  right = <evaluate b>
	  left = right
	LEND:
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
	
	free_reg(comp, right);
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
    
      left  = <code for left>
      right = <code for right>
      diff  = left - right
      goto_if NOTEQUAL, diff # not zero
      result = 1
      goto END
    NOTEQUAL:
      result = 0
    END:
    
    */
    m1_reg reg, 
           left, 
           right;
           
    int endlabel, 
        eq_ne_label;
    
    gencode_expr(comp, b->left);
    left = popreg(comp->regstack);
    
    gencode_expr(comp, b->right);
    right = popreg(comp->regstack);
    
    endlabel    = gen_label(comp);
    eq_ne_label = gen_label(comp);
    
    reg = alloc_reg(comp, VAL_INT);
    
    fprintf(OUT, "\tsub_i\tI%d, %c%d, %c%d\n", reg.no, reg_chars[(int)left.type], left.no,
                                                      reg_chars[(int)right.type], right.no);
                                                      
    fprintf(OUT, "\tgoto_if L%d, %c%d\n", eq_ne_label, reg_chars[(int)reg.type], reg.no);
    fprintf(OUT, "\tset_imm\t%c%d, 0, %d\n", reg_chars[(int)reg.type], reg.no, is_eq_op);
    fprintf(OUT, "\tgoto L%d\n", endlabel);                                                      
    
    fprintf(OUT, "L%d:\n", eq_ne_label);
    fprintf(OUT, "\tset_imm\t%c%d, 0, %d\n", reg_chars[(int)reg.type], reg.no, !is_eq_op);
    fprintf(OUT, "L%d:\n", endlabel);
    
    free_reg(comp, left);
    free_reg(comp, right);
    
    pushreg(comp->regstack, reg);
}


/*

Common code generation for < and <= operators. Note that the operands <left> 
and <right> are swapped! Therefore, even though this code is _very_ similar
to other binary math ops (which also handle isgt and isge), this routine
is different. This /could/ be refactored but it's better for clarity it's not.

*/
static void
lt_le_common(M1_compiler *comp, m1_binexpr *b, char const * const op) {
    m1_reg result = alloc_reg(comp, VAL_INT);
    m1_reg left, right;
    
    gencode_expr(comp, b->left);
    left = popreg(comp->regstack);
    
    gencode_expr(comp, b->right);
    right = popreg(comp->regstack);
    
    /* using isgt or isge ops, but swap arguments; hence, right first, then left. */
    fprintf(OUT, "\t%s_%c I%d, %c%d, %c%d\n", op, type_chars[(int)left.type], result.no, 
                                                  reg_chars[(int)right.type], right.no,
                                                  reg_chars[(int)left.type], left.no);

    free_reg(comp, left);
    free_reg(comp, right);
    pushreg(comp->regstack, result);    
}


static void
gencode_binary_bitwise(M1_compiler *comp, m1_binexpr *b, char const * const op) {
    m1_reg left, right, target;
    
    gencode_expr(comp, b->left);
    left = popreg(comp->regstack);

    gencode_expr(comp, b->right);  
    right  = popreg(comp->regstack);
    
    target = alloc_reg(comp, (m1_valuetype)left.type);    
    fprintf(OUT, "\t%s \t%c%d, %c%d, %c%d\n", op, reg_chars[(int)target.type], target.no, 
                                                  reg_chars[(int)left.type], left.no, 
                                                  reg_chars[(int)right.type], right.no);
    free_reg(comp, left);
    free_reg(comp, right);
    pushreg(comp->regstack, target);                    
    
}



/*

Common code generation routine for common math operators and > and >= operators.
The parameter <op> is the "simple" version, as in "add" (as opposed to "add_i").
The type suffice (_i, _n) is deduced from the left operand. It is up to the
type checker (see semcheck.c) that left and right are compatible.

*/
static void
gencode_binary_math(M1_compiler *comp, m1_binexpr *b, char const * const op) {
    m1_reg left, 
           right, 
           target;
    
    gencode_expr(comp, b->left);
    left = popreg(comp->regstack);
    
    gencode_expr(comp, b->right);  
    right  = popreg(comp->regstack);
    
    target = alloc_reg(comp, (m1_valuetype)left.type);    
    fprintf(OUT, "\t%s_%c\t%c%d, %c%d, %c%d\n", op, type_chars[(int)left.type],
                                                reg_chars[(int)target.type], target.no, 
                                                reg_chars[(int)left.type], left.no, 
                                                reg_chars[(int)right.type], right.no);
    free_reg(comp, left);
    free_reg(comp, right);
    pushreg(comp->regstack, target);               
}



static void
gencode_binary(M1_compiler *comp, m1_binexpr *b) {
    switch(b->op) {
        case OP_PLUS:
            gencode_binary_math(comp, b, "add");
            break;            
        case OP_MINUS:
            gencode_binary_math(comp, b, "sub");    
            break;            
        case OP_MUL:
            gencode_binary_math(comp, b, "mult");
            break;
        case OP_DIV:
            gencode_binary_math(comp, b, "div");
            break;            
        case OP_MOD:
            gencode_binary_math(comp, b, "mod");
            break;            
        case OP_GT:
            gencode_binary_math(comp, b, "isgt");
            break;            
        case OP_GE:
            gencode_binary_math(comp, b, "isge");
            break;            
        case OP_LT:
            lt_le_common(comp, b, "isgt"); /* swapping arguments. */
            break;
        case OP_LE:
            lt_le_common(comp, b, "isge");
            break;
        case OP_EQ:
            ne_eq_common(comp, b, 1);  /* 1 means is_eq_op is true. */
            break;
        case OP_NE: /* a != b;*/
            ne_eq_common(comp, b, 0); /* 0 means is_eq_op is false. */
            break;
        case OP_AND: /* a && b */
            gencode_and(comp, b);
            break;
        case OP_OR: /* a || b */
            gencode_or(comp, b);
            break;
        case OP_BAND:
            gencode_binary_bitwise(comp, b, "and");
            break;            
        case OP_BOR:
            gencode_binary_bitwise(comp, b, "or");
            break;
        case OP_XOR:
            gencode_binary_bitwise(comp, b, "xor");
            break;                        
        case OP_LRSH:
            gencode_binary_bitwise(comp, b, "lshr");
            break;
        case OP_RSH:
            gencode_binary_bitwise(comp, b, "ashr");
            break;
        case OP_LSH:
            gencode_binary_bitwise(comp, b, "shl");
            break;
        default:
            fprintf(stderr, "unknown operator\n");
            assert(0); /* should never happen. */
            break;   
    }                                
}


static void
gencode_not(M1_compiler *comp, m1_unexpr *u) {
    m1_reg reg, 
           temp;
           
    int label1, 
        label2;
    
    gencode_expr(comp, u->expr);
    reg  = popreg(comp->regstack);  
    temp = alloc_reg(comp, VAL_INT);
      
    /* If reg is zero, make it nonzero (false->true).
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
    
    free_reg(comp, reg);
    pushreg(comp->regstack, temp);
   
}

static void 
gencode_bnot(M1_compiler *comp, m1_unexpr *u) {
    /* Binary not (~x) is implemented as -x- 1). 
       Leave this in case M0 gets native support. 
     */
    assert(comp != NULL);
    assert(u != NULL);
    assert(0);
}

static void
gencode_unary(M1_compiler *comp, NOTNULL(m1_unexpr *u)) {
    char  *op;
    int    postfix = 0;
    m1_reg reg, 
           oldval; 
    
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
            gencode_not(comp, u);
            return; /* return after gencode_not(). */
        case UNOP_BNOT:
            gencode_bnot(comp, u);
            return; 
        default:
            fprintf(stderr, "unknown unary operator. Bailing out\n");
            assert(0);
            break;   
    }   
    
    
    /* generate code for the pre/post ++ expression */ 
    gencode_expr(comp, u->expr);
    reg = popreg(comp->regstack);
    
    /* register to hold the value "1". */        
    m1_reg one = alloc_reg(comp, VAL_INT);
    
    /* if it's a postfix op, then need to save the old value. */
    if (postfix == 1) {
        oldval = alloc_reg(comp, VAL_INT);
        fprintf(OUT, "\tset\tI%d, I%d, x\n", oldval.no, reg.no);
    }
    
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n", one.no);
    fprintf(OUT, "\t%s\tI%d, I%d, I%d\n", op, reg.no, reg.no, one.no);    
    
    if (postfix == 1) { /* postfix; give back the register containing the OLD value. */
    	pushreg(comp->regstack, oldval);    	
        free_reg(comp, reg);
    }
    else { /* prefix; give back the register containing the NEW value. */
        pushreg(comp->regstack, reg);
    }

    /* release the register that was holding the constant "1". */
    free_reg(comp, one);       
            
}

static void
gencode_continue(M1_compiler *comp) {	
    /* get label to jump to */
    int continuelabel = top(comp->continuestack);
	
    /* pop label from compiler's label stack (todo!) and jump there. */
    fprintf(OUT, "\tgoto\tL%d\n", continuelabel);
}

static void
gencode_break(M1_compiler *comp) {
    /* get label to jump to */
    int breaklabel = top(comp->breakstack);
    
    /* pop label from compiler's label stack (todo!) and jump there. */
    fprintf(OUT, "\tgoto\tL%d\n", breaklabel);
}

/* Generate sequence for a function call, including setting arguments
 * and retrieving return value.
 * XXX this function needs a bit of refactoring, cleaning up and comments.
 */
static void
gencode_funcall(M1_compiler *comp, m1_funcall *f) {
    m1_symbol *fun = f->funsym;
    
    assert(f->funsym != NULL);
    /* XXX figure out why this lookup is still needed. 
    has to do with fun->constindex
    */

    fun = sym_find_chunk(&comp->currentchunk->constants, f->name);
//    assert(fun == f->funsym);    
    /* XXX enable this as soon as it's resolved. */
    //assert(fun->constindex == f->funsym->constindex);
        
    m1_reg pc_reg, cont_offset;

     
    m1_reg cf_reg   = alloc_reg(comp, VAL_CHUNK);
    m1_reg sizereg  = alloc_reg(comp, VAL_INT);
    m1_reg flagsreg = alloc_reg(comp, VAL_INT);
    
    /* create a new call frame */
    /* alloc_cf: */
    fprintf(OUT, "\tset_imm   I%d, 0, 198\n", sizereg.no);
//    fprintf(OUT, "\tset_imm   I%d, 1, 0\n", sizereg.no); /* XXX: why $2 = 8 ? */
    fprintf(OUT, "\tset_imm   I%d, 0, 0\n", flagsreg.no);
    fprintf(OUT, "\tgc_alloc  P%d, I%d, I%d\n", cf_reg.no, sizereg.no, flagsreg.no);
    free_reg(comp, sizereg);
    free_reg(comp, flagsreg);

    
    /* store arguments in registers of new callframe.
       XXX this still needs to be specced for M0's calling conventions. */
       
    m1_expression *argiter = f->arguments;

 
    int regindexes[4] = { M0_REG_I0, 
                          M0_REG_N0, 
                          M0_REG_S0, 
                          M0_REG_P0};
    
    /* For each argument:
    
    set_imm IX, 0, <new register> # get value of argument type's register.
    set_ref PY, IX, RZ # index the CF to call with that index and copy the argument into it.
    
    */
    while (argiter != NULL) {
        m1_reg argreg;
        m1_reg indexreg = alloc_reg(comp, VAL_INT);
        gencode_expr(comp, argiter);
        argreg = popreg(comp->regstack);
        fprintf(OUT, "\tset_imm   I%d, 0, %d\n", indexreg.no, regindexes[argreg.type]);
        fprintf(OUT, "\tset_ref   P%d, I%d, %c%d\n", cf_reg.no, indexreg.no, 
                                                     reg_chars[(int)argreg.type], argreg.no);

        regindexes[argreg.type]++;
        
        argiter = argiter->next;   
    
        /* indexreg should NOT be unused. XXX need to find out why. 
        free_reg(comp, indexreg);
        */
        free_reg(comp, argreg);
        
    }

    
    /* init_cf_copy: */
    m1_reg temp = alloc_reg(comp, VAL_INT);    
    fprintf(OUT, "\tset_imm   I%d, 0, INTERP\n", temp.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, INTERP\n", cf_reg.no, temp.no);
    
    fprintf(OUT, "\tset_imm   I%d, 0, CHUNK\n", temp.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, CHUNK\n", cf_reg.no, temp.no);
    
    fprintf(OUT, "\tset_imm   I%d, 0, CONSTS\n", temp.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, CONSTS\n", cf_reg.no, temp.no);
    
    fprintf(OUT, "\tset_imm   I%d, 0, MDS\n", temp.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, MDS\n", cf_reg.no, temp.no);
    
    fprintf(OUT, "\tset_imm   I%d, 0, BCS\n", temp.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, BCS\n", cf_reg.no, temp.no);

    fprintf(OUT, "\tset_imm   I%d, 0, PCF\n", temp.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, CF\n", cf_reg.no, temp.no);
    
    fprintf(OUT, "\tset_imm   I%d, 0, CF\n", temp.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, P%d\n", cf_reg.no, temp.no, cf_reg.no);
    
    /* init_cf_zero: */
    m1_reg temp2 = alloc_reg(comp, VAL_INT);
    fprintf(OUT, "\tset_imm   I%d, 0, 0\n", temp.no);
    fprintf(OUT, "\tset_imm   I%d, 0, EH\n", temp2.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, I%d\n", cf_reg.no, temp2.no, temp.no); 

    fprintf(OUT, "\tset_imm   I%d, 0, RETPC\n", temp2.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, I%d\n", cf_reg.no, temp2.no, temp.no);

    fprintf(OUT, "\tset_imm   I%d, 0, SPILLCF\n", temp2.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, I%d\n", cf_reg.no, temp2.no, temp.no);

    free_reg(comp, temp2);

    /* init_cf_retpc: */    
    fprintf(OUT, "\tset_imm   I%d, 0, 10\n", temp.no);
    fprintf(OUT, "\tadd_i     RETPC, PC, I%d\n", temp.no);

    free_reg(comp, temp);

    cont_offset = alloc_reg(comp, VAL_INT);
    pc_reg      = alloc_reg(comp, VAL_INT);
    
    /* init_cf_pc */
    fprintf(OUT, "\tset_imm   I%d, 0, 3\n", cont_offset.no);
    fprintf(OUT, "\tadd_i     I%d, I%d, PC\n", cont_offset.no, cont_offset.no);
    fprintf(OUT, "\tset_imm   I%d, 0, PC\n", pc_reg.no);
    fprintf(OUT, "\tset_ref   P%d, I%d, I%d\n", cf_reg.no, pc_reg.no, cont_offset.no); 

    free_reg(comp, cont_offset);
    free_reg(comp, pc_reg);

    fprintf(OUT, "\tset       CF, P%d, x\n", cf_reg.no);
     
     
    /* post_set:   
    # put the name of the target chunk into S0
    set_imm P0, 0, 3
    deref   P0, CONSTS, P0
    # put the target PC into I0
    set_imm I0, 0, 0
    goto_chunk P0, I0, x
*/

    int calledfun_index = fun->constindex;
    fprintf(OUT, "\tset_imm    P%d, 0, %d\n", cf_reg.no, calledfun_index);
    fprintf(OUT, "\tderef      P%d, CONSTS, P%d\n", cf_reg.no, cf_reg.no);
    
    m1_reg I0 = alloc_reg(comp, VAL_INT);    
    fprintf(OUT, "\tset_imm    I%d, 0, 0\n", I0.no);
    fprintf(OUT, "\tgoto_chunk P%d, I%d, x\n", cf_reg.no, I0.no);
    free_reg(comp, I0);


    /*
    # We're back, so fix the parent call frame's PC and activate it.
    # The current CF's CHUNK, CONSTS, etc are updated by goto_chunk, so use
    # those values to update PCF.
    */
    
    /*
    retpc:
    restore_cf:
    */

    m1_reg I9 = alloc_reg(comp, VAL_INT);  
/*
    # set PCF[CHUNK] to the current call frame's CHUNK
    set_imm  I9,  0,  CHUNK
    set_ref  PCF, I9, CHUNK
    # set PCF[CONSTS] to the current call frame's CONSTS
    set_imm  I9,  0,  CONSTS 
    set_ref  PCF, I9, CONSTS
    # set PCF[MDS] to the current call frame's MDS
    set_imm  I9,  0,  MDS
    set_ref  PCF, I9, MDS
    # set PCF[BCS] to the current call frame's BCS
    set_imm  I9,  0,  BCS
    set_ref  PCF, I9, BCS
*/
    fprintf(OUT, "\tset_imm   I%d, 0, CHUNK\n", I9.no);
    fprintf(OUT, "\tset_ref   PCF, I%d, CHUNK\n", I9.no);
    
    fprintf(OUT, "\tset_imm   I%d, 0, CONSTS\n", I9.no);
    fprintf(OUT, "\tset_ref   PCF, I%d, CONSTS\n", I9.no);
    
    fprintf(OUT, "\tset_imm   I%d, 0, MDS\n", I9.no);
    fprintf(OUT, "\tset_ref   PCF, I%d, MDS\n", I9.no);
    
    fprintf(OUT, "\tset_imm   I%d, 0, BCS\n", I9.no);
    fprintf(OUT, "\tset_ref   PCF, I%d, BCS\n", I9.no);
    

    /* set_cf_pc: */
    /*
    # Set PCF[PC] to the invoke_cf + 1 so that when we invoke PCF with
    # "set CF, PCF, x", control flow will continue at the next instruction.
    */
    /*
    set_imm I1,  0,  5
    add_i   I1,  PC, I1
    set_imm I9,  0,  PC
    set_ref PCF, I9, I1
    set_imm I9,  0,  CF
    set_ref PCF, I9, PCF
    */
    m1_reg I1 = alloc_reg(comp, VAL_INT);    
    fprintf(OUT, "\tset_imm   I%d, 0,   5\n", I1.no);
    fprintf(OUT, "\tadd_i     I%d, PC,  I%d\n", I1.no, I1.no);
    fprintf(OUT, "\tset_imm   I%d, 0,   PC\n", I9.no);
    fprintf(OUT, "\tset_ref   PCF, I%d, I%d\n", I9.no, I1.no);
    fprintf(OUT, "\tset_imm   I%d, 0,   CF\n", I9.no);         
    fprintf(OUT, "\tset_ref   PCF, I%d, PCF\n", I9.no);
    free_reg(comp, I9);
    free_reg(comp, I1);
    /* invoke_cf: */
    
    /*
    set     CF, PCF, x
    */
    fprintf(OUT, "\tset       CF, PCF, x\n");
    

    
    /* generate code to get the return value. */
    /*
     set_imm IX, 0, R0  # get the number of register R0 into IX
     deref   RY, PZ, IX # index the callee's CF with that index, and store result in RY
    
    */
    /* retrieve the return value. */
    m1_reg idxreg           = alloc_reg(comp, VAL_INT);
    m1_reg retvaltarget_reg = alloc_reg(comp, f->typedecl->valtype);
    /* load the number of register I0. */
    fprintf(OUT, "\tset_imm\tI%d, 0, %c0\n", idxreg.no, reg_chars[(int)retvaltarget_reg.type]);   
    
    /* index the callee's frame (Px) with the index _of_ register X0. 
       That's where the callee left any return value. 
     */
    fprintf(OUT, "\tderef\t%c%d, P%d, I%d\n", reg_chars[(int)retvaltarget_reg.type], retvaltarget_reg.no, 
                                              cf_reg.no, idxreg.no);
                                               
    /* make it available for use by another statement. */
    pushreg(comp->regstack, retvaltarget_reg);
    
    /* we're accessing the callee's CF, so only free its register now.*/
    free_reg(comp, cf_reg);
          
}


static void
gencode_print_arg(M1_compiler *comp, m1_expression *expr) {
    if (expr == NULL) 
        return;
    
    /* go recursively to next, start at end of list as list is in reverse order. */    
    gencode_print_arg(comp, expr->next);
    
    /* now do the the real work for the current expression. */
    gencode_expr(comp, expr);    
    m1_reg reg = popreg(comp->regstack);
    m1_reg one = topreg(comp->regstack);
        
    fprintf(OUT, "\tprint_%c\tI%d, %c%d, x\n", type_chars[(int)reg.type], one.no, 
                                               reg_chars[(int)reg.type], reg.no);
                                               
    free_reg(comp, reg);
        
}

static void
gencode_print(M1_compiler *comp, m1_expression *expr) {    
    m1_reg one;
        
    one = alloc_reg(comp, VAL_INT);    
    fprintf(OUT, "\tset_imm\tI%d, 0, 1\n",  one.no);
    
    pushreg(comp->regstack, one); /* make reg holding "1" available to helper routine... */
    
    /* call helper routine. */
    gencode_print_arg(comp, expr);        

    (void)popreg(comp->regstack); /* .. and remove register holding 1. */
    free_reg(comp, one);
}

static void
gencode_new(M1_compiler *comp, m1_newexpr *expr) {
	m1_reg pointerreg = alloc_reg(comp, VAL_INT); /* reg holding the pointer to new memory */
	m1_reg sizereg    = alloc_reg(comp, VAL_INT); /* reg holding the num. of bytes to alloc. */

	unsigned size     = type_get_size(expr->typedecl);

    assert(size != NULL); /* this should never happen. */
    		
	fprintf(OUT, "\tset_imm I%d, 0, %d\n", sizereg.no, size);
	fprintf(OUT, "\tgc_alloc\tI%d, I%d, 0\n", pointerreg.no, sizereg.no);
	
	free_reg(comp, sizereg);
	pushreg(comp->regstack, pointerreg);
}

static void
gencode_exprlist(M1_compiler *comp, m1_expression *expr) {
    m1_expression *iter = expr;
    while (iter != NULL) {
        gencode_expr(comp, iter);
        iter = iter->next;   
    }    
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
    m1_case *caseiter;
    m1_reg   reg;    
    m1_reg   test     = alloc_reg(comp, VAL_INT);    
    int      endlabel = gen_label(comp);

    
    /* evaluate selector */
    gencode_expr(comp, expr->selector);
    reg = popreg(comp->regstack);
    
    push(comp->breakstack, endlabel); /* for break statements to jump to. */    

    /* iterate over cases and generate code for each. */
    caseiter = expr->cases;    
    while (caseiter != NULL) {
        int testlabel;
        /* XXX TODO handle numbers > 255 */
        /* reuse register "test". */
        fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", test.no, caseiter->selector);
        fprintf(OUT, "\tsub_i\tI%d, I%d, I%d\n", test.no, reg.no, test.no);
     
        testlabel = gen_label(comp);
        fprintf(OUT, "\tgoto_if L%d, I%d\n", testlabel, test.no);
        /* generate code for this case's block. */
        gencode_exprlist(comp, caseiter->block);
        /* next test label. */
        fprintf(OUT, "L%d:\n", testlabel);
        
        caseiter = caseiter->next;   
    }

    free_reg(comp, test);
    free_reg(comp, reg);
    
    if (expr->defaultstat) {
       gencode_expr(comp, expr->defaultstat); 
    }
    
    fprintf(OUT, "L%d:\n", endlabel);      
    (void)pop(comp->breakstack);
    
    
}

static void
gencode_var(M1_compiler *comp, m1_var *v) {    
    if (v->num_elems == 1 && v->init) { /* generate code for initializations. */
       m1_reg     reg;
       m1_symbol *sym;
       
       /* generate code for initialisation) */
       gencode_expr(comp, v->init);     
       reg = popreg(comp->regstack);
              
       assert(v->sym != NULL);
       sym = v->sym;              
       
       if (sym->regno == NO_REG_ALLOCATED_YET) {
            sym->regno = reg.no;
            freeze_reg(comp, reg);
       }
       else { /* no point in free()ing if just frozen; hence else clause. */
           free_reg(comp, reg);
       }
              
    }
    
    if (v->num_elems > 1) { /* generate code to allocate memory on the heap for arrays */
        m1_symbol *sym;
        m1_reg     memsize;                
        int        elem_size = 4; /* XXX fix this. Size of one element in the array. */
        int        size;

        sym = v->sym;
        assert(sym != NULL);
        
        if (sym->regno == NO_REG_ALLOCATED_YET) {
            m1_reg reg = alloc_reg(comp, sym->valtype);
            sym->regno = reg.no;
            freeze_reg(comp, reg);
        }
        
        /* calculate total size of array. If smaller than 256*255,
         * then load the value with set_imm, otherwise from the 
         * constants segment.
         */
        size = v->num_elems * elem_size;
        
        memsize = alloc_reg(comp, VAL_INT);
              
        if (size < (256*255)) {
            int remainder = size % 256;   
            int num256    = (size - remainder) / 256;
            fprintf(OUT, "\tset_imm\tI%d, %d, %d\n", memsize.no, num256, remainder);
        }
        else {
            m1_symbol *sizesym = sym_find_int(&comp->currentchunk->constants, size);
            m1_reg indexreg = alloc_reg(comp, VAL_INT);
            assert(sizesym != NULL);
            /* XXX this will fail if the const index in the CONSTS segment > 255. */
            fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", indexreg.no, sizesym->constindex); 
            fprintf(OUT, "\tderef\tI%d, CONSTS, I%d\n", memsize.no, indexreg.no);
            free_reg(comp, indexreg);
        }
        
        fprintf(OUT, "\tgc_alloc\tI%d, I%d, 0\n", sym->regno, memsize.no);
        free_reg(comp, memsize);
        
        if (v->init) { /* initialize arrays. */
            m1_expression *iter  = v->init;
            m1_reg         index = alloc_reg(comp, VAL_INT);
            m1_reg         one   = alloc_reg(comp, VAL_INT);
            unsigned elem_count  = 0;
            
            fprintf(OUT, "\tset_imm\tI%d, 0, 0\n", index.no); /* index register. */
            fprintf(OUT, "\tset_imm\tI%d, 0, 1\n", one.no);   /* to hold constant 1. */
            
            while (iter != NULL) {
                /* check for array bounds. */
                ++elem_count; /* XXX do this in semchec. */
                if (elem_count > v->num_elems) {
                    fprintf(stderr, "Error: too many elements for array of size %d\n", v->num_elems);
                    ++comp->errors;   
                }
                
                /* evaluate expression. */
                int numregs = gencode_expr(comp, iter);
                assert(numregs == 1);
                /* get register holding result. */
                m1_reg res = popreg(comp->regstack);
                
                /* and assign to array. */
                fprintf(OUT, "\tset_ref\tI%d, I%d, %c%d\n", sym->regno, index.no, reg_chars[(int)res.type], res.no);
                /* increment index. */
                fprintf(OUT, "\tadd_i\tI%d, I%d, I%d\n", index.no, index.no, one.no); 
                
                iter = iter->next;
            }    
            free_reg(comp, index);
            free_reg(comp, one);
        }
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
    reg    = popreg(comp->regstack);
    result = alloc_reg(comp, expr->targettype);
    
    switch (expr->targettype) {
        case VAL_INT:
            fprintf(OUT, "\tconvert_i_n\tI%d, %c%d, x\n", result.no, reg_chars[(int)reg.type], reg.no);
            break;
        case VAL_FLOAT:
            fprintf(OUT, "\tconvert_n_i\tN%d, %c%d, x\n", result.no, reg_chars[(int)reg.type], reg.no);
            break;
        default:
            assert(0);
            break;
    }

    free_reg(comp, reg);
    pushreg(comp->regstack, result);
  
}

static unsigned
gencode_expr(M1_compiler *comp, m1_expression *e) {
    unsigned num_regs = 1;
             
    if (e == NULL) { /* XXX should check on e before invoking gencode_expr(). */
    	debug("expr e is null in gencode_expr\n");
    	return 0;
    }
        
    switch (e->type) {
        case EXPR_ADDRESS:
            gencode_address(comp, e->expr.t);
            break;
        case EXPR_ASSIGN:
            gencode_assign(comp, e->expr.a);
            break;
        case EXPR_BINARY:
            gencode_binary(comp, e->expr.b);
            break;
        case EXPR_BLOCK:
            gencode_block(comp, e->expr.blck);
            break;
        case EXPR_BREAK:
            gencode_break(comp);
            break;
        case EXPR_CONTINUE:
            gencode_continue(comp);
            break;
        case EXPR_CAST:
            gencode_cast(comp, e->expr.cast);
            break;   
        case EXPR_CHAR:
            gencode_char(comp, e->expr.l);
            break;         
        case EXPR_CONSTDECL:
            /* do nothing. constants are compiled away */
        	break;            
        case EXPR_DEREF:
            gencode_deref(comp, e->expr.t);
            break;            
        case EXPR_DOWHILE:
            gencode_dowhile(comp, e->expr.w);
            break;
        case EXPR_FALSE:
            gencode_bool(comp, 0);
            break;              
        case EXPR_FOR:
            gencode_for(comp, e->expr.o);
            break;                      
        case EXPR_FUNCALL:
            gencode_funcall(comp, e->expr.f);
            break;
        case EXPR_IF:   
            gencode_if(comp, e->expr.i);
            break;            
        case EXPR_INT:
            gencode_int(comp, e->expr.l);
            break;
        case EXPR_NEW:
        	gencode_new(comp, e->expr.n);
        	break;    
        case EXPR_NULL:
            gencode_null(comp);
            break;
        case EXPR_NUMBER:
            gencode_number(comp, e->expr.l);
            break;
        case EXPR_OBJECT: 
        {
            m1_object *obj; /* temp. storage for a **pointer; 
                               we're not using the value of <obj>, 
                               only its space on the C runtime stack. 
                             */
            unsigned dimension_dummy = 0;
            num_regs = gencode_obj(comp, e->expr.t, &obj, &dimension_dummy, 0);            
            //fprintf(stderr, "generated code for object: %s\n", obj->obj.name);
            //fprintf(stderr, "symbol of %s is: %s\n", obj->obj.name, obj->sym->name);
            
            if (num_regs == 2) { /* gencode_obj() may return 2 registers for array and struct access. */
                m1_reg index  = popreg(comp->regstack);                
                m1_reg parent = popreg(comp->regstack);
                
                /* string s = stringarray[10]; -> an element of stringarray is therefore a string. */

                assert(obj != NULL);
                assert(obj->sym != NULL);
                assert(obj->sym->typedecl != NULL);
                
                m1_decl *target_type = obj->sym->typedecl;
                m1_reg target = alloc_reg(comp, target_type->valtype); 
                
                fprintf(OUT, "\tderef\t%c%d, %c%d, %c%d\n", reg_chars[(int)target.type], target.no, 
                                                            reg_chars[(int)parent.type], parent.no,
                                                            reg_chars[(int)index.type], index.no);
                                                            
                pushreg(comp->regstack, target); 
                --num_regs; /* popped 2, pushed 1. */
            }
            break;
        }
        case EXPR_PRINT:
            gencode_print(comp, e->expr.e);   
            break; 
        case EXPR_RETURN:
            gencode_return(comp, e->expr.e);
            break;            
        case EXPR_STRING:
            gencode_string(comp, e->expr.l);     
            break;
        case EXPR_SWITCH:
            gencode_switch(comp, e->expr.s);
        	break;    
        case EXPR_TRUE:
            gencode_bool(comp, 1);
            break;
        case EXPR_UNARY:
            gencode_unary(comp, e->expr.u);
            break;
        case EXPR_VARDECL:
            gencode_vardecl(comp, e->expr.v);            
            break;
        case EXPR_WHILE:
            gencode_while(comp, e->expr.w);
            break;        
         default:
            fprintf(stderr, "unknown expr type (%d)", e->type);   
            assert(0);
    }  
//    fprintf(stderr, "Returning from gen expr\n");
    return num_regs; 

}


/* Generate the constants segment. */
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
				fprintf(stderr, "unknown symbol type (%d)\n", iter->valtype);
				assert(0); /* should never happen. */
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
gencode_block(M1_compiler *comp, m1_block *block) {
    m1_expression *iter = block->stats;
    
    assert(&block->locals != NULL);
    /* set current symtab to this block's symtab. */
    comp->currentsymtab = &block->locals;
    
    /* iterate over block's statements and generate code for each. */
    while (iter != NULL) {
        gencode_expr(comp, iter);
        iter = iter->next;
    }  
    
    /* restore parent scope. */
    comp->currentsymtab = block->locals.parentscope;
}

static void
gencode_chunk_return(M1_compiler *comp, m1_chunk *chunk) {
    /*
    # figure out return PC and chunk
    # P0 is the parent call frame
    set_imm I3, 0,  PCF
    deref   P0, CF, I3
    # I4 is the parent call frame's RETPC
    set_imm I4, 0,  RETPC
    deref   I4, P0, I4
    # S3 is the parent call frame's CHUNK
    set_imm I3, 0,  CHUNK
    deref   I3, CONSTS, I3
    goto_chunk I3, I4, x
    */   
    
    /* XXX only generate in non-main functions. */
    
    /* XXX only generate if not already generated for an explicit return statement. */ 
    if (strcmp(chunk->name, "main") != 0) {        
        m1_reg chunk_index;
        m1_reg retpc_reg   = alloc_reg(comp, VAL_INT);
        m1_reg retpc_index = alloc_reg(comp, VAL_INT);

        fprintf(OUT, "\tset_imm    I%d, 0, RETPC\n", retpc_index.no);
        fprintf(OUT, "\tderef      I%d, PCF, I%d\n", retpc_reg.no, retpc_index.no);

        free_reg(comp, retpc_index);

        chunk_index = alloc_reg(comp, VAL_INT);

        fprintf(OUT, "\tset_imm    I%d, 0, CHUNK\n", chunk_index.no);
        fprintf(OUT, "\tderef      I%d, PCF, I%d\n", chunk_index.no, chunk_index.no);
        fprintf(OUT, "\tgoto_chunk I%d, I%d, x\n", chunk_index.no, retpc_reg.no);        

        free_reg(comp, retpc_reg);
        free_reg(comp, chunk_index);
    }
}

static void
gencode_parameters(M1_compiler *comp, m1_chunk *chunk) {
    m1_var *paramiter = chunk->parameters;
    fprintf(stderr, "[gencode] parameters for chunk (%d)\n", chunk->num_params);
    
    if (chunk->num_params > 0)
        assert(paramiter != NULL);
    
            
    while (paramiter != NULL) {
        /* get a new reg for this parameter. */
        m1_reg r = alloc_reg(comp, paramiter->sym->valtype); 
        paramiter->sym->regno = r.no;
        freeze_reg(comp, r); /* parameters are like local variables; they keep their register. */

//        fprintf(stderr, "Frozen reg %d for parameter %s\n", r.no, paramiter->name);
        
        paramiter = paramiter->next;   
    }
}


static void 
gencode_chunk(M1_compiler *comp, m1_chunk *c) {
#define PRELOAD_0_AND_1     0

    fprintf(OUT, ".chunk \"%s\"\n", c->name);    

    /* for each chunk, reset the register allocator */
    reset_reg(comp);
        
    gencode_consts(&c->constants);
    gencode_metadata(c);
    
    fprintf(OUT, ".bytecode\n");  
    
        
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
    
    gencode_parameters(comp, c);
    /* generate code for statements */
    gencode_block(comp, c->block);
    
    /* helper function to generate instructions to return. */
    gencode_chunk_return(comp, c);
}

/* Generate a function to setup the vtable. */
static void
gencode_pmc_vtable(M1_compiler *comp, m1_pmc *pmc) {
    m1_chunk *methoditer = pmc->methods;
    
    /* add methods to this special init chunk's const table. 
       Since this chunk is generated in code and not from the AST,
       manually reset comp's constindex first, otherwise the constant segment's
       entries get the wrong numbers.
     */
    comp->constindex = 0;
    while (methoditer != NULL) {
        sym_enter_chunk(comp, &comp->currentchunk->constants, methoditer->name);
        methoditer = methoditer->next;    
    }
    
    fprintf(OUT, ".chunk \"__%s_init_vtable__\"\n", pmc->name);
    gencode_consts(&comp->currentchunk->constants);
    fprintf(OUT, ".metadata\n");
    fprintf(OUT, ".bytecode\n");
    
    m1_reg indexreg      = alloc_reg(comp, VAL_INT);
    m1_reg vtablereg     = alloc_reg(comp, VAL_CHUNK);
    m1_reg methodreg     = alloc_reg(comp, VAL_CHUNK);

    
    
    int i = 0;
    /* allocate memory for a vtable. */
    fprintf(OUT, "\tset_imm\tI%d, 0, 100\n", indexreg.no);    
    fprintf(OUT, "\tgc_alloc\tP%d, I%d, x\n", vtablereg.no, indexreg.no);

    methoditer = pmc->methods;
    
    while (methoditer != NULL) {
        /* generate code to copy the pointer to the chunk into the vtable. */
        /* XXX can we do with a memcopy? */
        fprintf(OUT, "\tset_imm\tI%d, 0, %d\n", indexreg.no, i++);
        fprintf(OUT, "\tderef\tP%d, CONSTS, I%d\n", methodreg.no, indexreg.no);
        fprintf(OUT, "\tset_ref\tP%d, I%d, P%d\n", vtablereg.no, indexreg.no, methodreg.no);
        methoditer = methoditer->next;   
    }
    
    /* XXX need to generate code to return the vtable object. */
    
    free_reg(comp, methodreg);
    free_reg(comp, indexreg);
       
}

static void
gencode_pmc(M1_compiler *comp, m1_pmc *pmc) {

    
    /* generate code for the methods. */
    m1_chunk *methoditer = pmc->methods;
    while (methoditer != NULL) {
        /* set current chunk to this method. */
        comp->currentchunk = methoditer;   
        gencode_chunk(comp, methoditer);
        methoditer = methoditer->next;   
    }    
    
    /* XXX generate code for initialization, setting up vtables etc.*/    
    gencode_pmc_vtable(comp, pmc);
}

/*

Top-level function to drive the code generation phase.
Iterate over the list of chunks, and generate code for each.

*/
void 
gencode(M1_compiler *comp, m1_chunk *ast) {
    m1_chunk *iter = ast;
    m1_decl *decliter;
                            
    fprintf(OUT, ".version 0\n");
    
    while (iter != NULL) {     
        /* set pointer to current chunk, so that the code generator 
           has access to anything that belongs to the chunk. 
         */
        comp->currentchunk = iter;   
        gencode_chunk(comp, iter);
        iter = iter->next;   
    }
    
    /* after the normal chunks, generate code for PMC methods. */
    decliter = comp->declarations;
    while (decliter != NULL) {
        /* find the PMC definitions. */
        if (decliter->decltype == DECL_PMC) {
            m1_pmc *pmc = decliter->d.p;    
            gencode_pmc(comp, pmc);
        }
        decliter = decliter->next;   
    }
}



