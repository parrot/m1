#ifndef __M1_DECL_H__
#define __M1_DECL_H__

#include "ast.h"
#include "compiler.h"
#include "symtab.h"


/* declaration types. For each type, built-in or user-defined,
   a declaration type is stored in the compiler object. This
   enumeration defines the types of declarations. 
 */
typedef enum m1_decl_type {
    DECL_STRUCT,        /* a struct declaration */
    DECL_PMC,           /* a PMC declaration */
    DECL_ENUM,          /* an enumeration */
    DECL_INT,           /* declaration type for int type */
    DECL_NUM,           /* declaration type for num type */   
    DECL_STRING,        /* declaration type for string type */
    DECL_BOOL,          /* declaration type for bool type */
    DECL_VOID,          /* declaration type for void type */
    DECL_CHAR           /* declaration type for char type */
        
} m1_decl_type;



/* structure representing a type declaration. A new type declaratio
   is created for each struct, enum or PMC definition. Built-in types
   are "registered" before compilation starts, and for each of the built-in
   types, an m1_decl object is created as well, so these types can be found
   by name.
 */
typedef struct m1_decl {
    char *name;    /* name of declared type. */
    
    /* The information stored for a type definition differs per type. 
       For built-ins, it's just the size; if that changes that should become a struct
       as well. 
     */
    union { 
        struct m1_struct *s;    /* struct declaration. */
        struct m1_pmc    *p;    /* PMC declaration */
        struct m1_enum   *e;    /* enum declaration */
        unsigned          size; /* size of basic type whenever DECL_INT, DECL_FLOAT, DECL_STRING. */
    } d;
    
    m1_decl_type    decltype;   /* selector for union d */
    m1_valuetype    valtype;    /* type of register to hold this in. */
    
    struct m1_decl *next;   /* declarations are stored in a list. */
    
} m1_decl;

extern void print_type(m1_decl *type);

extern m1_decl *type_find_def(M1_compiler *, char *type);
extern m1_decl *type_enter_struct(M1_compiler *comp, char *structname, struct m1_struct *structdef);
extern m1_decl *type_enter_pmc(M1_compiler *comp, char *pmcname, struct m1_pmc *pmcdef);

extern m1_decl *type_enter_type(M1_compiler *comp, char *type, m1_decl_type decltype, unsigned size);
extern m1_decl *type_enter_enum(M1_compiler *comp, char *enumname, struct m1_enum *enumdef);

extern unsigned type_get_size(m1_decl *decl);

#endif

