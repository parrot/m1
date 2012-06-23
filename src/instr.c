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

*/
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include "instr.h"
#include "compiler.h"
#include "gencode.h"

char const * const m0_instr_names[] = {
    "noop",
    "goto",
    "goto_if",
    "goto_chunk",
    "add_i",
    "add_n",
    "sub_i",
    "sub_n",
    "mult_i",
    "mult_n",
    "div_i",
    "div_n",
    "mod_i",
    "mod_n",
    "iton",
    "ntoi",
    "ashr",
    "lshr",
    "shl",
    "and",
    "or",
    "xor",
    "gc_alloc",
    "sys_alloc",
    "sys_free",
    "copy_mem",
    "set",
    "set_imm",
    "deref",
    "set_ref",
    "set_byte",
    "get_byte",
    "set_word",
    "get_word",
    "csym",
    "ccall_arg",
    "ccall_ret",
    "ccall",
    "print_s",
    "print_i",
    "print_n",
    "exit"
    
};

static const char regs[REG_TYPE_NUM + 2] = {'I', 'N', 'S', 'P', ' ', 'L'};


#define OUT stdout



static void
write_instr(M1_compiler *comp, m0_instr *i) {
    
    
    switch (i->numops) {
        case 0:
            fprintf(OUT, "   %s\n", m0_instr_names[(int)i->opcode]); 
            break;
        case 1:                                                          
            fprintf(OUT, "   %s\t%c%d, x, x\n", m0_instr_names[(int)i->opcode], 
                                              regs[i->operands[0].type], i->operands[0].value);
 
            break;
        case 2:                                                             
            fprintf(OUT, "   %s\t%c%d, %c%d, x\n", m0_instr_names[(int)i->opcode], 
                                              regs[i->operands[0].type], i->operands[0].value,
                                              regs[i->operands[1].type], i->operands[1].value);                                              
            break;
        case 3:
            fprintf(OUT, "   %s\t%c%d, %c%d, %c%d\n", m0_instr_names[(int)i->opcode], 
                                              regs[i->operands[0].type], i->operands[0].value,
                                              regs[i->operands[1].type], i->operands[1].value,
                                              regs[i->operands[2].type], i->operands[2].value);
            break;
        default:
            fprintf(stderr, "too many operands for instruction");
            assert(0);
        }                                                                                                        
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

static m0_instr *
new_instr(M1_compiler *comp) {
    m0_instr *ins = (m0_instr *)calloc(1, sizeof (m0_instr));
    
    if (ins == NULL) {
        fprintf(stderr, "cant alloc mem for instr");
        exit(EXIT_FAILURE);
    }    
    return ins;
}

m0_instr *
mk_instr(M1_compiler *comp, m0_opcode opcode, char const * const format, ...) {
    va_list     argp;
    char const *p = NULL;
    int index     = 0;    
    m0_instr *ins = NULL; 
    
    
    if (comp->lastgenerated == NULL) 
        ins = new_instr(comp);            
    else {
        /* NOOP is never used, except if we had an instruction made already for a label.
           This is a signal that we can use that node, rather than making a new one.
        */
        if (comp->lastgenerated->opcode == M0_NOOP)            
            ins = comp->lastgenerated;        
        else 
            ins = new_instr(comp);           
    }
       
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
    
    comp->lastgenerated = ins;
    
//    write_instr(comp, ins);
    return ins;
    
}

void 
mk_label(M1_compiler *comp, unsigned labelno) {
    m0_instr *ins       = new_instr(comp);
    ins->label          = labelno;            
    comp->lastgenerated = ins;
    
    /* set this instruction's opcode to "noop", which is a signal to mk_instr
       that this node can be used for the next instruction.       
     */
    ins->opcode = M0_NOOP;       
}

m0_chunk *
mk_chunk(M1_compiler *comp, char *name) {
    m0_chunk *ch = (m0_chunk *)calloc(1, sizeof(m0_chunk));
    ch->name = name;
    return ch;   
}


