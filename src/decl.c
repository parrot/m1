#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "decl.h"


m1_decl *
type_find_def(M1_compiler *comp, char *typename) {
    m1_decl *iter;
    
    assert(comp != NULL);

    iter = comp->declarations;
    
    while (iter != NULL) {
        assert(iter->name != NULL);
        assert(typename != NULL);
        
        if (strcmp(iter->name, typename) == 0) { /* found! */
            return iter;
        }
        iter = iter->next;    
    }
    
    return NULL;    
}

m1_decl *
type_enter_struct(M1_compiler *comp, char *structname, struct m1_struct *structdef) {
    m1_decl *decl = (m1_decl *)calloc(1, sizeof(m1_decl));    
    
    assert(comp != NULL);
    
    fprintf(stderr, "entering type [%s]\n", structname);
    if (decl == NULL) {
        fprintf(stderr, "cant alloc mem for decl\n");
        exit(EXIT_FAILURE);   
    }
    
    decl->name = structname;
    decl->d.s  = structdef;
    decl->type = DECL_STRUCT;
    
    /* link in list of declarations */
    decl->next = comp->declarations;
    comp->declarations = decl;
    
    return decl;
}
