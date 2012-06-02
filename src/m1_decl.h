#ifndef __M1_DECL_H__
#define __M1_DECL_H__

#include "m1_compiler.h"

typedef enum m1_decl_type {
    DECL_STRUCT,
    DECL_PMC,
    DECL_ENUM
        
} m1_decl_type;

typedef struct m1_decl {
    char           *name;
    
    union {
        struct m1_struct *s; 
        struct m1_struct *p; /* for PMC declarations */
        struct m1_enum   *e;

    } d;
    
    m1_decl_type    type;
    struct m1_decl *next;
    
} m1_decl;


extern m1_decl *type_find_def(M1_compiler *comp, char *typename);
extern m1_decl *type_enter_struct(M1_compiler *comp, char *structname, struct m1_struct *structdef);

#endif

