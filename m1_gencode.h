#ifndef __M1_GENCODE_H__
#define __M1_GENCODE_H__

#include "m1_ast.h"
#include "m1_symtab.h"
/* to pass registers; I42 -> no=42, type='I' */
typedef struct m1_reg {
    unsigned char no;
    char          type;
} m1_reg;

extern void gencode(m1_chunk *ast);

#endif

