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

static const char types[REG_TYPE_NUM] = {'i', 'n', 's', 'p'};
static const char regs[REG_TYPE_NUM + 1] = {'I', 'N', 'S', 'P', ' '};


#define OUT stdout

/*
static void
write_noop(m0_instr *i) {
    fprintf(OUT, "noop");    
}

static void
write_goto(m0_instr *i) {
    fprintf(OUT, "goto");   
}

static void
write_goto_if(m0_instr *i) {
    fprintf(OUT, "goto_if");   
}

static void
write_goto_chunk(m0_instr *i) {
    fprintf(OUT, "goto_chunk");   
}

static void
write_add_i(m0_instr *i) {
    fprintf(OUT, "add_i");    
}

static void
write_add_n(m0_instr *i) {
    fprintf(OUT, "add_n");    
}

static void
write_sub_i(m0_instr *i) {
    fprintf(OUT, "sub_i");    
}

static void
write_sub_n(m0_instr *i) {
    fprintf(OUT, "sub_n");    
}

static void
write_mult_i(m0_instr *i) {
    fprintf(OUT, "mult_i");    
}

static void
write_mult_n(m0_instr *i) {
    fprintf(OUT, "mult_n");    
}

static void
write_div_i(m0_instr *i) {
    fprintf(OUT, "div_i");    
}

static void
write_div_n(m0_instr *i) {
    fprintf(OUT, "div_n");    
}

static void
write_mod_i(m0_instr *i) {
    fprintf(OUT, "mod_i");    
}

static void
write_mod_n(m0_instr *i) {
    fprintf(OUT, "mod_n");    
}

static void
write_iton(m0_instr *i) {
    fprintf(OUT, "iton");    
}
static void
write_ntoi(m0_instr *i) {
    fprintf(OUT, "ntoi");    
}
static void
write_ashr(m0_instr *i) {
    fprintf(OUT, "ashr");    
}
static void
write_lshr(m0_instr *i) {
    fprintf(OUT, "lshr");    
}
static void
write_shl(m0_instr *i) {
    fprintf(OUT, "shl");    
}

static void
write_and(m0_instr *i) {
    fprintf(OUT, "and");    
}

static void
write_or(m0_instr *i) {
    fprintf(OUT, "or");    
}

static void
write_xor(m0_instr *i) {
    fprintf(OUT, "xor");    
}

static void
write_gc_alloc(m0_instr *i) {
    fprintf(OUT, "gc_alloc");    
}

static void
write_sys_alloc(m0_instr *i) {
    fprintf(OUT, "sys_alloc");    
}
static void
write_sys_free(m0_instr *i) {
    fprintf(OUT, "sys_free");    
}

static void
write_copy_mem(m0_instr *i) {
    fprintf(OUT, "copy_mem");    
}

static void
write_set(m0_instr *i) {
    fprintf(OUT, "set");    
}

static void
write_set_imm(m0_instr *i) {
    fprintf(OUT, "set_imm");    
}

static void
write_deref(m0_instr *i) {
    fprintf(OUT, "deref");    
}

static void
write_set_ref(m0_instr *i) {
    fprintf(OUT, "set_ref");    
}

static void
write_set_byte(m0_instr *i) {
    fprintf(OUT, "set_byte");    
}
static void
write_get_byte(m0_instr *i) {
    fprintf(OUT, "get_byte");    
}
static void
write_set_word(m0_instr *i) {
    fprintf(OUT, "set_word");    
}
static void
write_get_word(m0_instr *i) {
    fprintf(OUT, "get_word");    
}
static void
write_csym(m0_instr *i) {
    fprintf(OUT, "csym");    
}

static void
write_ccall_arg(m0_instr *i) {
    fprintf(OUT, "ccall_arg");    
}

static void
write_ccall_ret(m0_instr *i) {
    fprintf(OUT, "ccall_ret");    
}

static void
write_ccall(m0_instr *i) {
    fprintf(OUT, "ccall");    
}

static void
write_print_s(m0_instr *i) {
    fprintf(OUT, "print_s");    
}
static void
write_print_i(m0_instr *i) {
    fprintf(OUT, "print_i");   
}

static void
write_print_n(m0_instr *i) {
    fprintf(OUT, "print_n");    
}

static void
write_exit(m0_instr *i) {
    fprintf(OUT, "exit");    
}
*/



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
instr(M1_compiler *comp, m0_opcode opcode, char const * const format, ...) {
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

        if (*p != '%') {
            continue;
        }

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
    va_end(argp);

    return ins;
    
}




/*
void
ins_label(M1_compiler *comp, unsigned labelno) {
    
}

void
ins_goto_if(M1_compiler *comp, unsigned labelno, char regtype, char regno) {
    
}

void
ins_goto(M1_compiler *comp, unsigned labelno) {
    
}

void
ins_set_imm(M1_compiler *comp, m1_reg *target, unsigned num256, unsigned numlt256) {
       
}

void
ins_deref(M1_compiler *comp, m1_reg *target, M0_alias obj, m1_reg *index) {
    
}
*/

