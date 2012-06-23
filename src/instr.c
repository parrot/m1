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
static const char regs[REG_TYPE_NUM + 2] = {'I', 'N', 'S', 'P', 'P', ' '};


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
    
    
    fprintf(comp->outfile, "   %s\t%c%d, %c%d, %c%d\n", m0_instr_names[(int)i->opcode], 
                                              regs[i->operands[0].type], i->operands[0].value,
                                              regs[i->operands[1].type], i->operands[1].value,
                                              regs[i->operands[2].type], i->operands[2].value);
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
/*
void
write_instructions(m0_instr *i) {
    while (i != NULL) {
        fprintf(OUT, "\t");
        switch (i->opcode) {
            case M0_NOOP: write_noop(i); break;
            case M0_GOTO: write_goto(i); break;
            case M0_GOTO_IF: write_goto_if(i); break;
            case M0_GOTO_CHUNK: write_goto_chunk(i); break;
            case M0_ADD_I: write_add_i(i); break;
            case M0_ADD_N: write_add_n(i); break;
            case M0_SUB_I: write_sub_i(i); break;            
            case M0_SUB_N: write_sub_n(i); break;
            case M0_MULT_I: write_mult_i(i); break;
            case M0_MULT_N: write_mult_n(i); break;
            case M0_DIV_I: write_div_i(i); break;
            case M0_DIV_N: write_div_n(i); break;
            case M0_MOD_I: write_mod_i(i); break;
            case M0_MOD_N: write_mod_n(i); break;
            case M0_ITON: write_iton(i); break;
            case M0_NTOI: write_ntoi(i); break;
            case M0_ASHR: write_ashr(i); break;
            case M0_LSHR: write_lshr(i); break;
            case M0_SHL: write_shl(i); break;
            case M0_AND: write_and(i); break;
            case M0_OR: write_or(i); break;
            case M0_XOR: write_xor(i); break;
            case M0_GC_ALLOC: write_gc_alloc(i); break;
            case M0_SYS_ALLOC: write_sys_alloc(i); break;
            case M0_SYS_FREE: write_sys_free(i); break;
            case M0_COPY_MEM: write_copy_mem(i); break;
            case M0_SET: write_set(i); break;
            case M0_SET_IMM: write_set_imm(i); break;
            case M0_DEREF: write_deref(i); break;
            case M0_SET_REF: write_set_ref(i); break;
            case M0_SET_BYTE: write_set_byte(i); break;
            case M0_GET_BYTE: write_get_byte(i); break;
            case M0_SET_WORD: write_set_word(i); break;
            case M0_GET_WORD: write_get_word(i); break;
            case M0_CSYM: write_csym(i); break;
            case M0_CCALL_ARG: write_ccall_arg(i); break;
            case M0_CCALL_RET: write_ccall_ret(i); break;
            case M0_CCALL: write_ccall(i); break;
            case M0_PRINT_S: write_print_s(i); break;
            case M0_PRINT_I: write_print_i(i); break;
            case M0_PRINT_N: write_print_n(i); break;
            case M0_EXIT: write_exit(i); break;
    
            default:
                fprintf(OUT, "unknown op!");
                exit(EXIT_FAILURE);
        }
        fprintf(OUT, "\n");
        i = i->next;   
    }   
}
*/

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
        /* skip all % markers. */
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

