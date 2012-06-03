#ifndef __M1_SYMTAB_H__
#define __M1_SYMTAB_H__


#include "compiler.h"

#define NO_REG_ALLOCATED_YET    (-1)

/* The valuetype enumeration lists essential the
   register types as needed in the M0 interpreter.
   This is different from a variable's type, which
   may be an object (a struct or PMC class's instance).

   Obviously, the built-in types each have their specialized
   valuetype, such as integers (VAL_INT) and so on.
   
   XXX Not sure about VALCHUNK VAL_ADDRESS and VAL_USERTYPE;
   may unify these. Will they map to P or I registers?
 */

typedef enum m1_valuetype {
	VAL_INT,
	VAL_FLOAT,
	VAL_STRING,
	VAL_CHUNK,   /* uses sval field of m1_value union */
	VAL_ADDRESS, /* uses ival field of m1_value union */
	VAL_USERTYPE
	
} m1_valuetype;



typedef union m1_value {
	char  *sval;
	double fval;
	int    ival;
	
} m1_value;


/* Structure for representing symbols in the symbol table, as well as constant
   declarations, which are just symbols in a "constants" symboltable. 
   
   The symbol struct has a pointer to a declaration node of a variable.
   So, for this declaration:
   
        int x;
   
   a var AST node is created, to represent the symbol in the AST, and a
   m1_symbol node is created, which is entered into the symbol table.
   Not all fields are always used. Only constants use the <constindex> field.
   Only variables get an allocated register in <regno>.
   
   The field <typedecl> is a pointer to the m1_decl node that represents
   the type of this symbol. Since symbols are stored by name (in a symboltable)
   these are easier to locate than m1_var nodes, which are just nodes in the
   abstract syntax tree (and therefore cannot easily be located).
   
 */
typedef struct m1_symbol {
    char             *name;     /* name of this symbol */
    m1_value          value;    /* for const declarations. */
    m1_valuetype      valtype;  /* selector of value union. */
    
    int               regno;    /* allocated register */
    int               scope;    /* scope of this symbol. */
    int               constindex;   /* index in const segment that holds this symbol's value. */
    struct m1_var    *var;          /* pointer to declaration AST node for var */
    struct m1_decl   *typedecl;     /* pointer to declaration of type. */
    
    struct m1_symbol *next;         /* symbols are stored in a list. */
    
} m1_symbol;



/* A symboltable is just a list of symbols, and a constant-counter, in case the 
   symbol represents a constant.   
 */
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

extern m1_symbol *sym_new_symbol(M1_compiler *comp, m1_symboltable *table, char *name, char *typename);
extern m1_symbol *sym_lookup_symbol(m1_symboltable *table, char *name);

extern void print_symboltable(m1_symboltable *table);

#endif

