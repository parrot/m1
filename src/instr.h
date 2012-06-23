#ifndef __M1_INSTR_H__
#define __M1_INSTR_H__

#include "compiler.h"

typedef enum m0_opcode {
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

} m0_opcode;

extern char const * const m0_instr_names[];


typedef enum m0_instr_flag {
    I_TYPE_INT   = 0x0001,
    I_TYPE_NUM   = 0x0002,
    I_TYPE_STR   = 0x0004,
    I_TYPE_PMC   = 0x0008,
    I_0_OPERANDS = 0x0010,
    I_1_OPERANDS = 0x0020,
    I_2_OPERANDS = 0x0040,
    I_3_OPERANDS = 0x0080
    
} m0_instr_flag;

    
typedef enum M0_alias {
    CF       = 0,
    PCF      = 1,
    PC       = 2,
    RETPC    = 3,
    EH       = 4,
    CHUNK    = 5,
    CONSTS   = 6,
    MDS      = 7,
    BCS      = 8,
    INTERP   = 9,
    SPC4RENT = 10,
    SPILLCF  = 11
    
} M0_alias;

typedef struct m0_operand {
    unsigned char value;
    unsigned char type;   
     
} m0_operand;


typedef struct m0_instr {
    char              opcode;
    char              flags;       /* maximum of 8 flags */
    unsigned int      label;       /* most instructions won't have one */
    struct m0_operand operands[3];
    
    struct m0_instr *next;
} m0_instr;

extern m0_instr *instr(M1_compiler *comp, m0_opcode, char const * const format, ...);


#endif

