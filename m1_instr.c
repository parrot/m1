#include "m1_instr.h"

#include <stdlib.h>
#include <stdio.h>

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

#define OUT stdout

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

/* XX other write functions go here. */


void
write_instructions(m0_instr *i) {
    while (i != NULL) {
        fprintf(OUT, "\t");
        switch (i->opcode) {
            case M0_NOOP: write_noop(i); break;
            case M0_GOTO: write_goto(i); break;
            case M0_GOTO_IF: write_goto_if(i); break;
            case M0_GOTO_CHUNK: write_goto_chunk(i); break;
            
            /* XXX todo: implement all functions to write. Also, fix operands at some point. 
    M0_ADD_I,
    M0_ADD_N,
    M0_SUB_I,
    M0_SUB_N,
    M0_MULT_I,
    M0_MULT_N,
    M0_DIV_I,
    M0_DIV_N,
    M0_MOD_I,
    M0_MOD_N,
    M0_ITON,
    M0_NTOI,
    M0_ASHR,
    M0_LSHR,
    M0_SHL,
    M0_AND,
    M0_OR,
    M0_XOR,
    M0_GC_ALLOC,
    M0_SYS_ALLOC,
    M0_SYS_FREE,
    M0_COPY_MEM,
    M0_SET,
    M0_SET_IMM,
    M0_DEREF,
    M0_SET_REF,
    M0_SET_BYTE,
    M0_GET_BYTE,
    M0_SET_WORD,
    M0_GET_WORD,
    M0_CSYM,
    M0_CCALL_ARG,
    M0_CCALL_RET,
    M0_CCALL,
    M0_PRINT_S,
    M0_PRINT_I,
    M0_PRINT_N,
    M0_EXIT
    */
            default:
                fprintf(OUT, "unknown op!");
                exit(EXIT_FAILURE);
        }
        fprintf(OUT, "\n");
        i = i->next;   
    }   
}

m0_instr *
instr(char op, char arg1, char arg2, char arg3) {
    
    m0_instr *i = (m0_instr *)calloc(1, sizeof (m0_instr));
    if (i == NULL) {
        fprintf(stderr, "cant alloc mem for instr");
        exit(EXIT_FAILURE);
    }
    
    i->opcode      = op;
    i->operands[0] = arg1;
    i->operands[1] = arg2;
    i->operands[2] = arg3;
    i->next        = NULL;
    
    return i;    
}
