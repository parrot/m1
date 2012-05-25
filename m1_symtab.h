#ifndef __M1_SYMTAB_H__
#define __M1_SYMTAB_H__



typedef enum m1_valuetype {
	VAL_STRING,
	VAL_FLOAT,
	VAL_INT,
	VAL_CHUNK /* uses str field of m1_value union */
} m1_valuetype;

typedef union m1_value {
	char  *str;
	double fval;
	int    ival;
	
} m1_value;

typedef struct m1_symbol {
    m1_value     value;
    int          regno; /* allocated register */
    m1_valuetype type;
    int          scope;
    int          constindex;
    struct m1_var    *var;
    struct m1_symbol *next;    
    
} m1_symbol;

typedef struct m1_symboltable {
    struct m1_symbol *syms;
    
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
extern void print_symboltable(m1_symboltable *table);

#endif

