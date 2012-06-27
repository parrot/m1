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
    M0_ISGT_I,
    M0_ISGT_N,
    M0_ISGE_I,
    M0_ISGE_N,
    M0_CONVERT_N_I,
    M0_CONVERT_I_N,
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

/* struct for a single M0 operand. */
typedef struct m0_operand {
    unsigned char value;
    unsigned char type;   
     
} m0_operand;

/* struct for a single instruction. */
typedef struct m0_instr {
    unsigned char     opcode;
    unsigned          numops;
    unsigned int      label;       /* most instructions won't have one */
    struct m0_operand operands[3];
    
    struct m0_instr *next;
} m0_instr;

/* struct representing an M0 chunk. */
typedef struct m0_chunk {
    char        *name;
    unsigned int num_instr; /* number of instructions in this chunk. */
    m0_instr *instructions; /* list of instructions. */
    
    struct m0_chunk *next;  /* chunks are stored in a list. */
    
} m0_chunk;

/* struct to represent an M0 file. */
typedef struct m0_file {
    unsigned int version;
    
    m0_chunk *chunks;
    
} m0_file;

extern m0_chunk *mk_chunk(M1_compiler *comp, char *name);

extern m0_instr *mk_instr(M1_compiler *comp, m0_opcode, char const * const format, ...);

extern void mk_label(M1_compiler *comp, unsigned labelno);

extern void write_chunk(M1_compiler *comp, struct m1_chunk *c);
extern void write_m0b_file(M1_compiler *comp);

#endif

