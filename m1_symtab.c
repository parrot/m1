/*


*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "m1_symtab.h"

m1_symboltable globals;
m1_symboltable floats;
m1_symboltable strings;
m1_symboltable ints;

int constindex = 0;

void
init_symtab(m1_symboltable *table) {
	globals.syms = NULL;
	floats.syms  = NULL;
	strings.syms = NULL;
	ints.syms    = NULL;	
}

static void
link_sym(m1_symboltable *table, m1_symbol *sym) {
	sym->next       = table->syms;
    table->syms     = sym;
    sym->constindex = constindex++;
}

m1_symbol *
sym_enter_str(m1_symboltable *table, char *name, int scope) {
    m1_symbol *sym;
    
    sym = sym_find_str(table, name);
    
    if (sym)
    	return sym;
    	
   	sym = (m1_symbol *)calloc(1, sizeof(m1_symbol));
    if (sym == NULL) {
        fprintf(stderr, "cant alloc mem for sym");
        exit(EXIT_FAILURE);
    }
    sym->value.str  = name;
    sym->type       = VAL_STRING;
    sym->scope      = scope;

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
    sym->type       = VAL_FLOAT;

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
    sym->type       = VAL_INT;    

    link_sym(table, sym);
    return sym;    
}

m1_symbol *
sym_find_str(m1_symboltable *table, char *name) {
    m1_symbol *sym = table->syms;
    
    while (sym != NULL) {
        if (strcmp(sym->value.str, name) == 0)
            return sym;
            
        sym = sym->next;   
    }
    return NULL;
}

m1_symbol *
sym_find_num(m1_symboltable *table, double fval) {
    m1_symbol *sym = table->syms;
    
    while (sym != NULL) {
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
