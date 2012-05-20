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


m0_instr *
instr(char op, char arg1, char arg2, char arg3) {
    
    m0_instr *i = (m0_instr *)calloc(1, sizeof (m0_instr));
    if (i == NULL) {
        fprintf(stderr, "cant alloc mem for instr");
        exit(EXIT_FAILURE);
    }
    i->opcode = op;
    i->operands[0] = arg1;
    i->operands[1] = arg2;
    i->operands[2] = arg3;
    return i;    
}
