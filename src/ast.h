#ifndef __M1_AST_H__
#define __M1_AST_H__

#include "symtab.h"
#include "decl.h"
#include "instr.h"
#include "compiler.h"

#include "ann.h"

/* for lists of identifiers. */
typedef struct m1_ident {
    char            *name;      
    struct m1_ident *next;    
    
} m1_ident;


/* for array dimensions. */
typedef struct m1_dimension {
    unsigned             num_elems;  /* number of elements in this dimension. */
    struct m1_dimension *next;       /* x[1][2][3] has 3 dimensions, which are 
                                        linked through next pointer. */
      
} m1_dimension;

typedef struct m1_block {
    struct m1_expression *stats;        /* list of statements in this block. */
    struct m1_symboltable locals;       /* variables declared in this block. */
    
} m1_block;

typedef enum chunk_flag {
    CHUNK_ISFUNCTION = 0x000,
    CHUNK_ISVTABLE   = 0x001,
    CHUNK_ISMETHOD   = 0x002
    
} chunk_flag;

/* structure to represent a function. */
typedef struct m1_chunk {
    char                 *rettype;      /* return type of chunk */
    char                 *name;         /* name of this chunk */    
    struct m1_chunk      *next;         /* chunks are stored in a list. */
    struct m1_block      *block;        /* list of statements. */

    struct m1_var        *parameters;   /* list of parameters */
    struct m1_symbol     *sym;          /* pointer to symbol table entry */
    unsigned              num_params;   /* parameter count. */
    
    int                   flags;
    
    unsigned              line;         /* line of function declaration. */    
    struct m1_symboltable constants;    /* constants used in this chunk */
        
} m1_chunk;


/* structure that holds an M1 struct definition */
typedef struct m1_struct {
    char    *name;              /* name of this struct. */
    int      is_union;          /* union declarations also use this AST node type. */
    unsigned size;              /* total size of this struct; can calculate from fields but 
                                   better keep a "cached" value */
    
    struct m1_symboltable sfields; /* a struct is just a very local scope; it's handy to have
                                      a symbol table, as it makes handling x.y.z easier. 
                                    */
    unsigned line_defined;

    struct m1_ident       *parents;
    struct m1_chunk       *methods;    

      
} m1_struct;


/* To represent "lhs = rhs" statements. */
typedef struct m1_assignment {
    struct m1_object     *lhs;
    struct m1_expression *rhs;
           
} m1_assignment;

/* To represent function call statements. */
typedef struct m1_funcall {
    char                 *name;
    struct m1_expression *arguments;
    struct m1_type       *typedecl;  /* type declaration for return type; needed for type checking */
    struct m1_symbol     *funsym; /* entry in symbol table for this function definition. */
    unsigned              constindex; /* index into CONSTS segment of the chunk from which this function is called. */
    
} m1_funcall;

/* To represent new expressions (new Object(x, y, z) ). */
typedef struct m1_newexpr {
	char                 *type;            /* name of new type to instantiate. */
	struct m1_type       *typedecl;        /* pointer to declaration of type. */
	struct m1_expression *args;            /* arguments passed on to type's constructor. */    
	
} m1_newexpr;


typedef enum m1_expr_type {
    EXPR_ADDRESS,   /* &x */
    EXPR_ASSIGN,
    EXPR_BINARY,
    EXPR_BLOCK,
    EXPR_BREAK,
    EXPR_CONTINUE,
    EXPR_CAST,
    EXPR_CHAR,    
    EXPR_CONSTDECL,
    EXPR_DEREF,     /* *x */
    EXPR_DOWHILE,
    EXPR_FALSE,
    EXPR_FOR,
    EXPR_FUNCALL,
    EXPR_IF,
    EXPR_INT,
    EXPR_M0BLOCK,
    EXPR_NEW,
    EXPR_NULL,
    EXPR_NUMBER,
    EXPR_OBJECT,
    EXPR_PRINT,   /* temporary? */    
    EXPR_RETURN,
    EXPR_STRING,
    EXPR_SWITCH,
    EXPR_TRUE,
    EXPR_UNARY,
    EXPR_VARDECL,
    EXPR_WHILE
    

} m1_expr_type;


typedef enum m1_binop {
	OP_ASSIGN,
    OP_PLUS,
    OP_MINUS,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_XOR,
    OP_GT,
    OP_GE,
    OP_LT,
    OP_LE,
    OP_EQ,
    OP_NE,
    OP_AND,
    OP_OR,
    OP_BAND,
    OP_BOR,
    OP_RSH,
    OP_LRSH,
    OP_LSH
} m1_binop;

/* To represent binary expressions, like a + b. */
typedef struct m1_binexpr {
    struct m1_expression *left;
    struct m1_expression *right;
    m1_binop op;
        
} m1_binexpr;


typedef enum m1_unop {
    UNOP_POSTINC,  /* a++ */
    UNOP_POSTDEC,  /* a-- */
    UNOP_PREINC,   /* ++a */
    UNOP_PREDEC,   /* --a */
    UNOP_NOT,      /* !a  */
    UNOP_BNOT      /* ~a  */
    /* There is no UNOP_NEG: unary minus is handled by multiplying by -1 */
} m1_unop;

/* for unary expressions, like -x, and !y. */
typedef struct m1_unexpr {
    struct m1_expression *expr;     
    m1_unop op;                     
    
} m1_unexpr;

typedef struct m1_castexpr {
    struct m1_expression *expr;
    char                 *type;    
    m1_valuetype          targettype;
} m1_castexpr;

/* object types. */
typedef enum m1_object_type {
    OBJECT_LINK,  /* node linking a and b in a.b; uses <field> field in obj union. */
    OBJECT_MAIN,  /* a in a.b  uses <name> field in obj union. */
    OBJECT_FIELD, /* b in a.b; uses <name> field in obj union. */
    OBJECT_INDEX, /* b in a[b]; uses <index> field in obj union */
    OBJECT_DEREF, /* b in a->b; NOT IMPLEMENTED */
    OBJECT_SCOPE, /* b in a::b; NOT IMPLEMENTED */
    OBJECT_SELF,  /* "self"     NOT IMPLEMENTED */
    OBJECT_SUPER  /* "super"    NOT IMPLEMENTED */
    
} m1_object_type;

/* struct to represent an element or link between two elements
   in aggregates. In a.b.c, each element (a, b, c) is represented
   by one m1_object node. Links, between a and b, and b and c are ALSO
   represented by a m1_object node. Yes, that's a lot of nodes for 
   an expression like "a.b.c" (5 in total).
   
 */
typedef struct m1_object {
    unsigned line;    
    
    union {
        char                 *as_name;  /* for name, field or deref access, in a.b.c for instance. */
        struct m1_expression *as_index; /* for array index (a[42]) */        
        struct m1_object     *as_field; /* if this is a linking node (OBJECT_LINK) representing "a.b" as a whole. */
    } obj;
    
    enum m1_object_type type;       /* selector for union */
    struct m1_symbol   *sym;        /* pointer to this object's declaration. */ 

    struct m1_object   *parent;     /* pointer to its parent (in a.b.c, a is b's parent) */
      
} m1_object;



/* for while and do-while statements */
typedef struct m1_whileexpr {
    struct m1_expression *cond;    
    struct m1_expression *block;
} m1_whileexpr;

/* for if statements */
typedef struct m1_ifexpr {
    struct m1_expression *cond;
    struct m1_expression *ifblock;
    struct m1_expression *elseblock;
} m1_ifexpr;

/* for for-statements */
typedef struct m1_forexpr {
    struct m1_expression *init;
    struct m1_expression *cond;
    struct m1_expression *step;
    struct m1_expression *block;    
} m1_forexpr;

/* AST node for const declarations */
typedef struct m1_const {
    char                 *type;
    char                 *name;
    struct m1_expression *value;
} m1_const;


/* variable declarations */
typedef struct m1_var {
    char                 *name;
    char                 *type;      /* store type name; type may not have been parsed yet; check in type checker. */
    struct m1_expression *init;      /* to handle: int x = 42; */
    unsigned              num_elems; /* 1 for non-arrays, larger for arrays */
    struct m1_symbol     *sym;       /* pointer to symbol in symboltable */
    struct m1_dimension  *dims;      /* pointer to list of dimensions, for arrays. */
    struct m1_var        *next;      /* var nodes are stored as a list. */
} m1_var;


/* structure to represent a single case of a switch statement. */
typedef struct m1_case {
	int                   selector;
	struct m1_expression *block;	
	struct m1_case       *next;
	
} m1_case;


/* structure to represent a switch statement. */
typedef struct m1_switch {
	struct m1_expression *selector;
	struct m1_case       *cases;
	struct m1_expression *defaultstat;
	
} m1_switch;

/* for representing literal constants, i.e., int, float and strings */
typedef struct m1_literal {
    union m1_value     value; /* the value */
    enum m1_valuetype  type; /* selector for the union value */
    struct m1_symbol  *sym; /* pointer to a symboltable entry. */
    
} m1_literal;

typedef struct m0_block {
    struct m0_instr       *instr;
       
} m0_block;

typedef struct m1_enumconst {
    char  *name;                /* name of this constant */
    int    value;               /* value of this constant */
    struct m1_enumconst *next;
} m1_enumconst;

typedef struct m1_enum {
    char         *enumname;
    m1_enumconst *enums;            /* list of enum constants. */
    
} m1_enum;

/* to represent statements */
typedef struct m1_expression {
    union {
        struct m1_unexpr     *as_unexpr;
        struct m1_binexpr    *as_binexpr;
        struct m1_funcall    *as_funcall;  
        struct m1_assignment *as_assign; 
        struct m1_whileexpr  *as_whileexpr;  
        struct m1_forexpr    *as_forexpr;
        struct m1_ifexpr     *as_ifexpr;
        struct m1_expression *as_expr; 
        struct m1_object     *as_object;
        struct m1_const      *as_const;
        struct m1_var        *as_var;
        struct m0_block      *as_m0_block;
        struct m1_switch     *as_switch;
        struct m1_newexpr    *as_newexpr;
        struct m1_literal    *as_literal;
        struct m1_castexpr   *as_cast;
        struct m1_block      *as_block;
    } expr;
    
    m1_expr_type  type; /* selector for union */
    unsigned      line; /* line number */
    
    struct m1_expression *next;
    
} m1_expression;


extern int yyget_lineno(yyscan_t yyscanner);

//extern m1_chunk *chunk(M1_compiler *comp, char *rettype, char *name);
extern m1_chunk *chunk(ARGIN_NOTNULL(M1_compiler * const comp), ARGIN(char *rettype), ARGIN_NOTNULL(char *name), int flags);

//extern m1_expression *block(M1_compiler *comp);
extern m1_block *block(ARGIN_NOTNULL(M1_compiler *comp));

extern m1_expression *expression(M1_compiler *comp, m1_expr_type type);       
extern m1_expression *funcall(M1_compiler *comp, m1_object *fun, m1_expression *args);
            
extern m1_object *object(M1_compiler *comp, m1_object_type type);            
extern void obj_set_ident(m1_object *node, char *ident);

extern m1_struct *newstruct(M1_compiler *comp, char *name, m1_ident *parents);

extern m1_expression *ifexpr(M1_compiler *comp, m1_expression *cond, m1_expression *ifblock, m1_expression *elseblock);
extern m1_expression *whileexpr(M1_compiler *comp, m1_expression *cond, m1_expression *block);
extern m1_expression *dowhileexpr(M1_compiler *comp, m1_expression *cond, m1_expression *block);
extern m1_expression *forexpr(M1_compiler *comp, m1_expression *init, m1_expression *cond, m1_expression *step, m1_expression *stat);

extern m1_expression *inc_or_dec(M1_compiler *comp, m1_expression *obj, m1_unop optype);
extern m1_expression *returnexpr(M1_compiler *comp, m1_expression *retexp);
extern m1_expression *assignexpr(M1_compiler *comp, m1_expression *lhs, int assignop, m1_expression *rhs);

extern m1_expression *objectexpr(M1_compiler *comp, m1_object *obj, m1_expr_type type);

extern m1_expression *binexpr(M1_compiler *comp, m1_expression *e1, int op, m1_expression *e2);
extern m1_expression *number(M1_compiler *comp, double value);
extern m1_expression *integer(M1_compiler *comp, int value);
extern m1_expression *character(M1_compiler *comp, char ch);

extern m1_expression *string(M1_compiler *comp, char *str);
extern m1_expression *unaryexpr(M1_compiler *comp, m1_unop op, m1_expression *e);
extern m1_object *arrayindex(M1_compiler *comp, m1_expression *index);
extern m1_object *objectfield(M1_compiler *comp, char *field);
extern m1_object *objectderef(M1_compiler *comp, char *field);
extern m1_expression *printexpr(M1_compiler *comp, m1_expression *e);
extern m1_expression *constdecl(M1_compiler *comp, char *type, char *name, m1_expression *expr);
extern m1_expression *vardecl(M1_compiler *comp, char *type, m1_var *v);

extern m1_var *var(M1_compiler *comp, char *name, m1_expression *init);
extern m1_var *make_var(M1_compiler *comp, char *varname, m1_expression *init, unsigned num_elems);

extern m1_var *array(M1_compiler *comp, char *name, m1_dimension *dimension, m1_expression *init);

extern unsigned field_size(struct m1_symbol *field);

extern m1_expression *switchexpr(M1_compiler *comp, m1_expression *expr, m1_case *cases, m1_expression *defaultstat);
extern m1_case *switchcase(M1_compiler *comp, int selector, m1_expression *block);

extern m1_expression *newexpr(M1_compiler *copm, char *type, m1_expression *args);

extern m1_object *lhsobj(M1_compiler *comp, m1_object *parent, m1_object *field);
extern m1_expression *castexpr(M1_compiler *comp, char *type, m1_expression *castedexpr);

extern m1_enumconst *enumconst(M1_compiler *comp, char *enumitem, int enumvalue);
extern m1_enum *newenum(M1_compiler *comp, char *name, m1_enumconst *enumconstants);

extern m1_var *parameter(M1_compiler *comp, char *type, char *name);


extern void block_set_stat(m1_block *block, m1_expression *stat);

extern struct m1_block *open_scope(M1_compiler *comp);
extern void close_scope(M1_compiler *comp);

extern m1_dimension *array_dimension(int ival);

extern m1_ident *identlist(m1_ident *next, char *newnode);

extern void add_chunk_parameters(M1_compiler *comp, m1_chunk *chunk, m1_var *paramlist, int flags);

#endif

