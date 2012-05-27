/*


*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "m1_symtab.h"



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
        fprintf(stderr, "cant alloc mem for new sym");
        exit(EXIT_FAILURE);   
    }
    sym->value.sval = name;
    sym->regno      = NO_REG_ALLOCATED_YET;
    sym->valtype    = type;
    
    link_sym(table, sym);
    
    return sym;   
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
sym_enter_str(m1_symboltable *table, char *name, int scope) {
    m1_symbol *sym;
    
    fprintf(stderr, "enter_str()\n");
    assert(table != NULL);
    
    sym = sym_find_str(table, name);
    
    if (sym) {
        fprintf(stderr, "string found\n");
    	return sym;
    }
    fprintf(stderr, "string not found\n");
    	
   	sym = (m1_symbol *)calloc(1, sizeof(m1_symbol));
    if (sym == NULL) {
        fprintf(stderr, "cant alloc mem for sym");
        exit(EXIT_FAILURE);
    }
    
    fprintf(stderr, "sym enter ");
    
    sym->value.sval = name;
    sym->valtype    = VAL_STRING;
    sym->scope      = scope;
    sym->constindex = table->constindex++;
    
    fprintf(stderr, "sym enter str done\n");
    link_sym(table, sym);
    return sym;    
}

m1_symbol *
sym_enter_chunk(m1_symboltable *table, char *name) {
    m1_symbol *sym;
    
    sym = sym_find_chunk(table, name);
    
    if (sym)
    	return sym;
    	
   	sym = (m1_symbol *)calloc(1, sizeof(m1_symbol));
    if (sym == NULL) {
        fprintf(stderr, "cant alloc mem for sym");
        exit(EXIT_FAILURE);
    }
    sym->value.sval = name;
    sym->valtype    = VAL_CHUNK;
    sym->constindex = table->constindex++;
    link_sym(table, sym);
    return sym;       
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
    if (sym)
    	return sym;
    	
    sym = (m1_symbol *)calloc(1, sizeof(m1_symbol));
    if (sym == NULL) {
        fprintf(stderr, "cant alloc mem for sym");
        exit(EXIT_FAILURE);
    }
    
    sym->value.ival = val;
    sym->valtype    = VAL_INT;    
    sym->constindex = table->constindex++;
    
    link_sym(table, sym);
    return sym;    
}

m1_symbol *
sym_find_str(m1_symboltable *table, char *name) {
    m1_symbol *sym;
    
    assert(table != NULL);
    
    sym = table->syms;
    
    
    while (sym != NULL) {
     
        fprintf(stderr, "comparing string\n");
        assert(sym->value.sval != NULL);
        
        if (strcmp(sym->value.sval, name) == 0) {
            fprintf(stderr, "done\n");
            return sym;
        }
            
        sym = sym->next;   
    }
    fprintf(stderr, "cant find string");
    return NULL;
}

m1_symbol *
sym_find_chunk(m1_symboltable *table, char *name) {
    m1_symbol *sym = table->syms;
    
    while (sym != NULL) {
        if (strcmp(sym->value.sval, name) == 0)
            return sym;
            
        sym = sym->next;   
    }
    return NULL;
}

m1_symbol *
sym_find_num(m1_symboltable *table, double fval) {
    m1_symbol *sym = table->syms;
    
    while (sym != NULL) {
        /* XXX shouldn't do comparisons on floats */
        if (sym->value.fval == fval) {        	
            return sym;            
        }
            
        sym = sym->next;   
    }
    return NULL;
}

m1_symbol *
sym_find_int(m1_symboltable *table, int ival) {
    m1_symbol *sym = table->syms;
    
    while (sym != NULL) {
        if (sym->value.ival == ival)
            return sym;
            
        sym = sym->next;   
    }
    return NULL;
}
