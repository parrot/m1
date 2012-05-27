#ifndef __M1_SYMTAB_H__
#define __M1_SYMTAB_H__

#define NO_REG_ALLOCATED_YET    (-1)

typedef enum m1_valuetype {
	VAL_INT,
	VAL_FLOAT,
	VAL_STRING,
	VAL_CHUNK /* uses str field of m1_value union */
} m1_valuetype;

typedef union m1_value {
	char  *sval;
	double fval;
	int    ival;
	
} m1_value;

typedef struct m1_symbol {
    char             *name; 
    m1_value          value;
    m1_valuetype      valtype;
    
    int               regno; /* allocated register */
    int               scope;
    int               constindex;
    struct m1_var    *var; /* pointer to declaration AST node for var */
    struct m1_symbol *next;    
    
} m1_symbol;

typedef struct m1_symboltable {
    struct m1_symbol *syms;
    int    constindex; /* one symboltable per constants segment, therefore keep
                           constindex local to the table. */
    
} m1_symboltable;



extern m1_symboltable *new_symtab(void);

extern m1_symbol *sym_enter_str(m1_symboltable *table, char *name, int scope);
extern m1_symbol *sym_enter_num(m1_symboltable *table, double val);
extern m1_symbol *sym_enter_int(m1_symboltable *table, int val);
extern m1_symbol *sym_enter_chunk(m1_symboltable *table, char *name);

extern m1_symbol *sym_find_str(m1_symboltable *table, char *name);
extern m1_symbol *sym_find_num(m1_symboltable *table, double val);
extern m1_symbol *sym_find_int(m1_symboltable *table, int val);
extern m1_symbol *sym_find_chunk(m1_symboltable *table, char *name);

extern m1_symbol *sym_new_symbol(m1_symboltable *table, char *name, int type);
extern m1_symbol *sym_lookup_symbol(m1_symboltable *table, char *name);

extern void print_symboltable(m1_symboltable *table);

#endif

