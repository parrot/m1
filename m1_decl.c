#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "m1_decl.h"


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
type_enter_def(M1_compiler *comp, char *typename) {
    m1_decl *decl = (m1_decl *)calloc(1, sizeof(m1_decl));    
    
    assert(comp != NULL);
    
    fprintf(stderr, "entering type [%s]\n", typename);
    if (decl == NULL) {
        fprintf(stderr, "cant alloc mem for decl\n");
        exit(EXIT_FAILURE);   
    }
    
    decl->name = typename;
    decl->next = comp->declarations;
    comp->declarations = decl;
    
    return decl;
}
