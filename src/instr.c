/* 

This file contains functions to construct and write instructions.
The constructor for m0_instr is a variadic function, which means
the number of arguments that is passed is encoded in a format string.
The format string specifiers are adapted from printf()'s and friends.
The following specifiers may be used:

 %R     to pass a m1_reg node, which holds both register type and number.
 %I     to pass an integer holding an I register number.
 %N     like %I, but for N registers.
 %S     like %I, but for S registers.
 %P     like %I, but for P registers.
 %d     to pass a integer literal.
 %L     to pass a label number.
 %X     to pass a special register (CONSTS, CF, etc.)

*/
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include "instr.h"
#include "compiler.h"
#include "gencode.h"

char const * const m0_instr_names[] = {
    "noop       ",
    "goto       ",
    "goto_if    ",
    "goto_chunk ",
    "add_i      ",
    "add_n      ",
    "sub_i      ",
    "sub_n      ",
    "mult_i     ",
    "mult_n     ",
    "div_i      ",
    "div_n      ",
    "mod_i      ",
    "mod_n      ",
    "isgt_i     ",
    "isgt_n     ",
    "isge_i     ",
    "isge_n     ",
    "convert_n_i",
    "convert_i_n",
    "ashr       ",
    "lshr       ",
    "shl        ",
    "and        ",
    "or         ",
    "xor        ",
    "gc_alloc   ",
    "sys_alloc  ",
    "sys_free   ",
    "copy_mem   ",
    "set        ",
    "set_imm    ",
    "deref      ",
    "set_ref    ",
    "set_byte   ",
    "get_byte   ",
    "set_word   ",
    "get_word   ",
    "csym       ",
    "ccall_arg  ",
    "ccall_ret  ",
    "ccall      ",
    "print_s    ",
    "print_i    ",
    "print_n    ",
    "exit       "
    
};

static char const * const interp_regs[] = {
    "CF",
    "PCF",
    "PC",
    "RETPC",
    "EH",
    "CHUNK",
    "CONSTS",
    "MDS",
    "BCS",
    "INTERP",
    "SPC4RENT",
    "SPILLCF"
};


static const char regs[REG_TYPE_NUM + 2] = {'I', 'N', 'S', 'P', ' ', 'L'};


#define OUT stdout
//comp->outfile


static void
write_operand(M1_compiler *comp, m0_operand op) {

    switch (op.type) {
        case VAL_INT:
            fprintf(OUT, "I%d", op.value);
            break;
        case VAL_FLOAT:
            fprintf(OUT, "N%d", op.value);
            break;        
        case VAL_STRING:
            fprintf(OUT, "S%d", op.value);
            break;
        case VAL_CHUNK:
            fprintf(OUT, "P%d", op.value);
            break;        
        case VAL_VOID:
            fprintf(OUT, "%d", op.value);
            break;
        case VAL_LABEL:
            fprintf(OUT, "L%d", op.value);
            break;            
        case VAL_INTERP_REG:
            fprintf(OUT, "%s", interp_regs[op.value]);
            break;
        default:
            fprintf(stderr, "unknown m0_instr operand\n");
            assert(0);
    }
}

static void
write_instr(M1_compiler *comp, m0_instr *i) {
    
    assert(comp != NULL);
    
    if (i->label != 0 && i->opcode == M0_NOOP) 
        fprintf(OUT, "L%d:\n", i->label);    
        
    if (i->opcode == M0_NOOP)
        return;
        
    fprintf(OUT, "\t%s ", m0_instr_names[(int)i->opcode]);
    unsigned index;
    for (index = 0; index < i->numops; index++) {
        write_operand(comp, i->operands[index]);   
        if (index < i->numops - 1)
            fprintf(OUT, ", ");
    }        
    
    if (i->opcode != M0_GOTO && i->opcode != M0_GOTO_IF)
        for (; index < 3; index++) 
            fprintf(OUT, ", x");
        
    fprintf(OUT, "\n");
                                                                                                           
}

void
write_instructions(M1_compiler *comp, m0_instr *i) {

    comp->outfile = fopen("a.m1", "w");
    
    if (comp->outfile == NULL) {
        fprintf(stderr, "Failed to open output file\n");
        exit(EXIT_FAILURE);   
    }
    
    while (i != NULL) {
        write_instr(comp, i);
        i = i->next;   
    }
    fclose(comp->outfile);
       
}

/* Allocate a new m0_instr node in memory. */
static m0_instr *
new_instr(M1_compiler *comp) {
    m0_instr *ins = (m0_instr *)calloc(1, sizeof (m0_instr));
    
    if (ins == NULL) {
        fprintf(stderr, "cant alloc mem for instr");
        exit(EXIT_FAILURE);
    }    
    return ins;
}

/* Get a new instruction node; it may already exist to store a label;
   if not, then a new instruction node is created.
   Whereever the instr node comes from, it's returned and usable
   by the instr constructor. 
*/
static m0_instr *
link_instr(M1_compiler *comp) {
    m0_instr *instr = NULL;
    
    /* comp keeps a pointer to the last generated one for quick access to the tail
       of the list, so that the new one can be added in O(1) time, rather than
       O(n) time.
     */
    if (comp->lastgenerated == NULL) 
        instr = new_instr(comp);  /* first instruction. */          
    else {
        /* If the last generated instruction was a "noop", then it was a label;
           noop is not generated normally, and is used as a dummy. Therefore,
           reuse it by overwriting its opcode.
         */
        if (comp->lastgenerated->opcode == M0_NOOP)            
            instr = comp->lastgenerated;        
        else /* last one was normal so just create a new instr node. */ 
            instr = new_instr(comp);           
        
        /* lastgenerated wasn't NULL, so link new instruction to it. */
        comp->lastgenerated->next = instr;
    }
    /* update "last generated instruction" pointer. */
    comp->lastgenerated = instr;  
    
    return instr;  
}

/* Constructor for m0_instr; the format string parameter specifies which operands to expect. */
m0_instr *
mk_instr(M1_compiler *comp, m0_opcode opcode, char const * const format, ...) {
    va_list     argp;
    char const *p     = NULL;
    int         index = 0;    
    m0_instr   *ins   = NULL;                

            
    ins         = link_instr(comp);
    ins->opcode = opcode;
                    
    va_start(argp, format);
           
    for (p = format; *p != '\0'; p++) {

        if (*p != '%') 
            continue;        

        switch (*++p) {
            case 'N':
                ins->operands[index].type  = VAL_FLOAT;
                ins->operands[index].value = va_arg(argp, int);
                break;                
            case 'I':
                ins->operands[index].type  = VAL_INT;
                ins->operands[index].value = va_arg(argp, int);                
                break;
            case 'S':
                ins->operands[index].type  = VAL_STRING;
                ins->operands[index].value = va_arg(argp, int);                
                break;
            case 'P':
                ins->operands[index].type  = VAL_CHUNK;                                
                ins->operands[index].value = va_arg(argp, int);                
                break;
            case 'R': {
                m1_reg r = va_arg(argp, m1_reg);
                ins->operands[index].type  = r.type;
                ins->operands[index].value = r.no;
                break;
            }
            case 'L': 
                ins->operands[index].type  = VAL_LABEL;
                ins->operands[index].value = va_arg(argp, int);
                break;
            case 'X':
                ins->operands[index].type  = VAL_INTERP_REG;
                ins->operands[index].value = va_arg(argp, int);
                break; 
            case 'd':
                ins->operands[index].type  = VAL_VOID;
                ins->operands[index].value = va_arg(argp, int);
    			break;

            default:
                fprintf(stderr, "unrecognized error reporting format\n");
                assert(0);
                
        }       
        ++index;
               
    }
    
    ins->numops = index;
    ins->next   = NULL;
    
    
    va_end(argp);
    

    
    write_instr(comp, ins);
    return ins;
    
}

/* Make an empty m0_instr node and store the specified label. */
void 
mk_label(M1_compiler *comp, unsigned labelno) {
    m0_instr *ins = link_instr(comp);
    
    ins->label    = labelno;            
    
    /* set this instruction's opcode to "noop", which is a signal to mk_instr
       that this node can be used for the next instruction.       
     */
    ins->opcode = M0_NOOP;   
    write_instr(comp, ins);    
}

m0_chunk *
mk_chunk(M1_compiler *comp, char *name) {
    m0_chunk *ch = (m0_chunk *)calloc(1, sizeof(m0_chunk));
    ch->name     = name;
    return ch;   
}

static void
write_consts(M1_compiler *comp, m1_symboltable *consttable) {
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

static void
write_metadata(M1_compiler *comp, m1_chunk *c) {
    assert(c != NULL);
	fprintf(OUT, ".metadata\n");	
}


void
write_chunk(M1_compiler *comp, m1_chunk *c) {
    fprintf(OUT, ".chunk \"%s\"\n", c->name);       
    
    write_consts(comp, &c->constants);
    write_metadata(comp, c);
    
    fprintf(OUT, ".bytecode\n");  

}

void
write_m0b_file(M1_compiler *comp) {
    fprintf(OUT, ".version 0\n");
       
}

