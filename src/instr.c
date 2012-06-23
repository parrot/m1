#include "instr.h"
#include "compiler.h"
#include "gencode.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>

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

static const char regs[REG_TYPE_NUM + 1] = {'I', 'N', 'S', 'P', ' '};


#define OUT stdout



static void
write_instr(M1_compiler *comp, m0_instr *i) {
    
    
    switch (i->numops) {
        case 0:
            fprintf(comp->outfile, "   %s\n", m0_instr_names[(int)i->opcode]); 
            break;
        case 1:                                                          
            fprintf(comp->outfile, "   %s\t%c%d, x, x\n", m0_instr_names[(int)i->opcode], 
                                              regs[i->operands[0].type], i->operands[0].value);
 
            break;
        case 2:                                                             
            fprintf(comp->outfile, "   %s\t%c%d, %c%d, x\n", m0_instr_names[(int)i->opcode], 
                                              regs[i->operands[0].type], i->operands[0].value,
                                              regs[i->operands[1].type], i->operands[1].value);                                              
            break;
        case 3:
            fprintf(comp->outfile, "   %s\t%c%d, %c%d, %c%d\n", m0_instr_names[(int)i->opcode], 
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

m0_instr *
mk_instr(M1_compiler *comp, m0_opcode opcode, char const * const format, ...) {
    va_list argp;
    char const *p;
    int index = 0;
    
    m0_instr *ins = (m0_instr *)calloc(1, sizeof (m0_instr));
    
    if (ins == NULL) {
        fprintf(stderr, "cant alloc mem for instr");
        exit(EXIT_FAILURE);
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
    
    /* if not first generated instruction, then set that item's next to the new instr. */
    if (comp->lastgenerated != NULL)
        comp->lastgenerated->next = ins;
    /* update last generated instruction for fast access. */        
    comp->lastgenerated = ins;
    
    va_end(argp);

    return ins;
    
}


m0_chunk *
mk_chunk(M1_compiler *comp) {
    m0_chunk *ch = NULL;
    return ch;   
}


