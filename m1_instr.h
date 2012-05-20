#ifndef __M1_INSTR_H__
#define __M1_INSTR_H__

typedef enum m0_instr_code {
    M0_NOOP,
    M0_GOTO,
    M0_GOTO_IF,
    M0_GOTO_CHUNK,
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

} m0_instr_code;

extern char const * const m0_instr_names[];

typedef struct m0_instr {
    char opcode;
    char operands[3];
    
    struct m0_instr *next;
} m0_instr;

extern m0_instr *instr(char op, char arg1, char arg2, char arg3);

#endif

