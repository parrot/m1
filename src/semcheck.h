#ifndef __M1_SEMCHECK_H__
#define __M1_SEMCHECK_H__

#include "compiler.h"
#include "ast.h"

typedef enum m1_type {
    TYPE_VOID,
    TYPE_INT,
    TYPE_BOOL,
    TYPE_NUM,
    TYPE_STRING,
    TYPE_CHUNK,
    TYPE_NULL
            
} m1_type;

extern void check(struct M1_compiler *comp, struct m1_chunk *ast);

#endif
