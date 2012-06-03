/*


*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "symtab.h"



m1_symboltable *
new_symtab(void) {
    m1_symboltable *table = (m1_symboltable *)calloc(1, sizeof (m1_symboltable));
    if (table == NULL) {
        fprintf(stderr, "cant alloc new symtab");
        exit(EXIT_FAILURE);   
    }
    table->syms = NULL;
    return table;   
}

/*

Add symbol C<sym> to symboltable C<table>. 

*/
static void
link_sym(m1_symboltable *table, m1_symbol *sym) {
    m1_symbol *iter;
    
    iter = table->syms;
    if (iter == NULL) {
        table->syms = sym;
    }
    else {
        /* go to end of list, and hook on symbol at the end */
        while (iter->next != NULL) {
            iter = iter->next;
        }      
        iter->next = sym;
    }

}

m1_symbol *
sym_new_symbol(m1_symboltable *table, char *name, int type) {
    m1_symbol *sym = (m1_symbol *)calloc(1, sizeof(m1_symbol));
    
    if (sym == NULL) {
        fprintf(stderr, "cant alloc mem for new sym %s", name);
        exit(EXIT_FAILURE);   
    }
    sym->name       = name; /* name of this symbol */
    sym->regno      = NO_REG_ALLOCATED_YET;
    sym->valtype    = type; 
    sym->next       = NULL;
    
    link_sym(table, sym);
    
    return sym;   
}

m1_symbol *
sym_lookup_symbol(m1_symboltable *table, char *name) {
    m1_symbol *sym;
    
    assert(table != NULL);
    assert(name != NULL);
    
    sym = table->syms;
        
    while (sym != NULL) {        
        assert(sym->name != NULL);
        assert(name != NULL);  
                           
        if (strcmp(sym->name, name) == 0) {     
            return sym;
        }   
            
        sym = sym->next;   
    }
    return NULL;
}


void
print_symboltable(m1_symboltable *table) {
    m1_symbol *iter = table->syms;
    fprintf(stderr, "SYMBOL TABLE\n");
    while (iter != NULL) {
        fprintf(stderr, "symbol '%s' has register %d and type %d\n", iter->value.sval, iter->regno, iter->valtype);
        iter = iter->next;   
    }   
}

m1_symbol *
sym_enter_str(m1_symboltable *table, char *str, int scope) {
    m1_symbol *sym;
    
    assert(table != NULL);
    
    sym = sym_find_str(table, str);
    
    if (sym) {
     
    	return sym;
    }
    	
   	sym = (m1_symbol *)calloc(1, sizeof(m1_symbol));
    if (sym == NULL) {
        fprintf(stderr, "cant alloc mem for sym");
        exit(EXIT_FAILURE);
    }        
    
    sym->value.sval = str;
    sym->valtype    = VAL_STRING;
    sym->scope      = scope;
    sym->constindex = table->constindex++;
    
    link_sym(table, sym);
    return sym;    
}

m1_symbol *
sym_enter_chunk(m1_symboltable *table, char *name) {
    m1_symbol *sym;
    /* a chunk is just stored as a name, but override the type. */
    sym = sym_enter_str(table, name, 0);        
    sym->valtype    = VAL_CHUNK;
    return sym;       
}

m1_symbol *
sym_find_chunk(m1_symboltable *table, char *name) {
    return sym_find_str(table, name);   
}

m1_symbol *
sym_enter_num(m1_symboltable *table, double val) {
    m1_symbol *sym;
    
    sym = sym_find_num(table, val);
    if (sym)
    	return sym;
    	
    sym = (m1_symbol *)calloc(1, sizeof(m1_symbol));
    if (sym == NULL) {
        fprintf(stderr, "cant alloc mem for sym");
        exit(EXIT_FAILURE);
    }
    
    sym->value.fval = val;
    sym->valtype    = VAL_FLOAT;
    sym->constindex = table->constindex++;
    
    link_sym(table, sym);
    
    return sym;    
}

m1_symbol *
sym_enter_int(m1_symboltable *table, int val) {
    m1_symbol *sym;
           
    sym = sym_find_int(table, val);
    if (sym) {
    	return sym;
    }
        	
    sym = (m1_symbol *)calloc(1, sizeof(m1_symbol));
    if (sym == NULL) {
        fprintf(stderr, "cant alloc mem for sym");
        exit(EXIT_FAILURE);
    }
    
    sym->value.ival = val;
    sym->valtype    = VAL_INT;    
    sym->constindex = table->constindex++;
    sym->next       = NULL;
    
    link_sym(table, sym);
    return sym;    
}

m1_symbol *
sym_find_str(m1_symboltable *table, char *name) {
    m1_symbol *sym;
    
    assert(table != NULL);
    assert(name != NULL);
    
    sym = table->syms;
        
    while (sym != NULL) {        
        if (sym->valtype == VAL_STRING || sym->valtype == VAL_CHUNK) {
            assert(sym->value.sval != NULL);
            assert(name != NULL);  
                           
            if (strcmp(sym->value.sval, name) == 0) {     
                return sym;
            }   
        }
            
        sym = sym->next;   
    }
    return NULL;
}



m1_symbol *
sym_find_num(m1_symboltable *table, double fval) {
    m1_symbol *sym = table->syms;
    
    while (sym != NULL) {
       
        if (sym->valtype == VAL_FLOAT) {
            if (sym->value.fval == fval) {        	
                return sym;            
            }
        }
            
        sym = sym->next;   
    }
    return NULL;
}

m1_symbol *
sym_find_int(m1_symboltable *table, int ival) {
    m1_symbol *sym = table->syms;
    
    while (sym != NULL) {
        if (sym->valtype == VAL_INT) {
            if (sym->value.ival == ival) {
                return sym;
            }
        }    
        sym = sym->next;   
    }
    return NULL;
}
