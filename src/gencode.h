#ifndef __M1_GENCODE_H__
#define __M1_GENCODE_H__

#include "ast.h"
#include "symtab.h"
#include "compiler.h"

/* to store registers; I42 -> no=42, type=VAL_INT */
typedef struct m1_reg {
    short no;         /* number of register. */
    short type;       /* type of register. */
    
} m1_reg;

extern void gencode(M1_compiler *comp, m1_chunk *ast);

#endif

