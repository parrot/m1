#ifndef __M1_DECL_H__
#define __M1_DECL_H__

#include "m1_compiler.h"

typedef enum m1_decl_type {
    DECL_STRUCT,
    DECL_PMC,
    DECL_ENUM
        
} m1_decl_type;

typedef struct m1_decl {
    char *name;
    
    m1_decl_type type;
    
} m1_decl;


extern m1_decl *type_find_def(M1_compiler *comp, char *typename);

#endif

