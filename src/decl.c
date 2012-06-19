#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "decl.h"
#include "ast.h"


void
print_type(m1_type *type) {
    fprintf(stderr, "TYPE: [%s]\n", type->name);   
}

/*

Find the declaration for type <typename>.

*/
m1_type *
type_find_def(M1_compiler *comp, char *type) {
    m1_type *iter;
    
    assert(comp != NULL);

    iter = comp->declarations;
    
    while (iter != NULL) {
        assert(iter->name != NULL);
        assert(type != NULL);
        
        if (strcmp(iter->name, type) == 0) { /* found! */
            return iter;
        }
        iter = iter->next;    
    }
    
    return NULL;    
}


static m1_type *
make_decl(M1_compiler *comp, int type) {
    m1_type *decl = (m1_type *)calloc(1, sizeof(m1_type));    
    
    assert(comp != NULL);
    
    if (decl == NULL) {
        fprintf(stderr, "cant alloc mem for decl\n");
        exit(EXIT_FAILURE);   
    }
    decl->decltype = type;
    return decl;
}

/*

Enter a new struct declaration that goes by name <structname>.

*/
m1_type *
type_enter_struct(M1_compiler *comp, char *structname, struct m1_struct *structdef) {
    m1_type *decl   = make_decl(comp, DECL_STRUCT);    
    decl->name      = structname;
    decl->d.s       = structdef;
    
    structdef->size = 4; /* XXX The size of a struct or PMC is always 4 bytes, as it's stored as a pointer. */
    
    /* link in list of declarations */
    decl->next = comp->declarations;
    comp->declarations = decl;
    
    return decl;
}

/*

Enter a new PMC declaration that goes by name <pmcname>.

*/
m1_type *
type_enter_pmc(M1_compiler *comp, char *pmcname, struct m1_pmc *pmcdef) {
    m1_type *decl = make_decl(comp, DECL_PMC);
    decl->name    = pmcname;
    decl->d.p     = pmcdef;

    pmcdef->size = 4; /* XXX fix size */
    
    /* link in list of declarations. */
    decl->next = comp->declarations;
    comp->declarations = decl;
    
    return decl;    
}

/*

Enter a new enumeration declaration that goes by <enumname>.

*/
m1_type *
type_enter_enum(M1_compiler *comp, char *enumname, struct m1_enum *enumdef) {
    m1_type *decl = make_decl(comp, DECL_ENUM);
    decl->name    = enumname;
    decl->d.e     = enumdef;
    
    decl->next = comp->declarations;
    comp->declarations = decl;
    
    return decl;   
}

/* 

Interface for declaring basic types. 

*/
m1_type *
type_enter_type(M1_compiler *comp, char *type, m1_type_type decltype, unsigned size) {
    m1_type *decl  = (m1_type *)calloc(1, sizeof(m1_type));    
    decl->name     = type;
    decl->decltype = decltype;    
    decl->d.size   = size;
    
    switch (decltype) {
        case DECL_INT:
            decl->valtype = VAL_INT;
            break;
        case DECL_BOOL:
            decl->valtype = VAL_INT;
            break;
        case DECL_NUM:
            decl->valtype = VAL_FLOAT;
            break;
        case DECL_STRING:
            decl->valtype = VAL_STRING;
            break;
        default:
            decl->valtype = VAL_INT; /* structs and PMCs are stored as pointers. */
            break;
    }

    decl->next = comp->declarations;
    comp->declarations = decl;
    
    return decl;        
}

/*

Get the size of the type in <decl>.

*/
unsigned
type_get_size(m1_type *decl) {
    int size;
    assert(decl != NULL);
    switch (decl->decltype) {
        case DECL_STRUCT:
            size = decl->d.s->size;
            break;
        case DECL_PMC:
            size = decl->d.p->size;
            break;
        case DECL_ENUM:
            fprintf(stderr, "Why do you need to know the size of an enum?\n");
            assert(0);
            break;
        /* built-in types: */    
        case DECL_INT:
        case DECL_NUM:
        case DECL_STRING:
        case DECL_BOOL:
            size = decl->d.size;
            break;
        case DECL_VOID:
            fprintf(stderr, "Cannot find size for type 'void'\n");
            assert(0);
            break;
        default:
            assert(0);
            break;
    }      
    return size;
}

