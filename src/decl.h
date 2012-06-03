#ifndef __M1_DECL_H__
#define __M1_DECL_H__

#include "compiler.h"

typedef enum m1_decl_type {
    DECL_STRUCT,
    DECL_PMC,
    DECL_ENUM,
    DECL_INT,
    DECL_NUM,
    DECL_STRING,
    DECL_BOOL,
    DECL_VOID
        
} m1_decl_type;

typedef struct m1_decl {
    char           *name;    /* name of declared type. */
    
    union {
        struct m1_struct *s;     /* struct declaration. */
        struct m1_struct *p;     /* for PMC declarations */
        struct m1_enum   *e;     /* enum declaration */
        unsigned          size;  /* size of basic type whenever DECL_INT, DECL_FLOAT, DECL_STRING. */
    } d;
    
    m1_decl_type    type;   /* selector for union d */
    
    struct m1_decl *next;   /* declarations are stored in a list. */
    
} m1_decl;


extern m1_decl *type_find_def(M1_compiler *comp, char *typename);
extern m1_decl *type_enter_struct(M1_compiler *comp, char *structname, struct m1_struct *structdef);
extern m1_decl *type_enter_type(M1_compiler *comp, char *typename, m1_decl_type type, unsigned size);

#endif

