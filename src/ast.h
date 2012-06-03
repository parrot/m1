#ifndef __M1_AST_H__
#define __M1_AST_H__

#include "symtab.h"
#include "decl.h"
#include "instr.h"
#include "compiler.h"



typedef struct m1_chunk {
    char                 *rettype;      /* return type of chunk */
    char                 *name;         /* name of this chunk */    
    struct m1_chunk      *next;         /* chunks are stored in a list. */
    struct m1_expression *block;        /* list of statements. */
    /* TODO: add parameters */
    
    struct m1_symboltable locals;       /* local vars in this chunk */
    struct m1_symboltable constants;    /* constants used in this chunk */
        
} m1_chunk;

typedef struct m1_structfield {
    char        *name; 
    char        *type;     
    unsigned     offset;
    
    struct m1_structfield *next;
    
} m1_structfield;

typedef struct m1_struct {
    char    *name;
    unsigned numfields;
    unsigned size; /* can calculate from fields but better keep a "cached" value */
    
    struct m1_structfield *fields;
    
    struct m1_struct *structs;
    
} m1_struct;


typedef struct m1_assignment {
    struct m1_object     *lhs;
    struct m1_expression *rhs;
    
} m1_assignment;

typedef struct m1_funcall {
    char *name;
    /* TODO: add args */
    
} m1_funcall;

typedef struct m1_newexpr {
	char *type;
	/* TODO handle args */
	
} m1_newexpr;


typedef enum m1_expr_type {
    EXPR_ADDRESS,   /* &x */
    EXPR_ASSIGN,
    EXPR_BINARY,
    EXPR_BREAK,
    EXPR_CAST,    
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
    OP_LSH
} m1_binop;

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
    UNOP_NOT       /* !a  */
    /* unary minus is handled by multiplying by -1 */
} m1_unop;

typedef struct m1_unexpr {
    struct m1_expression *expr;
    m1_unop op;
    
} m1_unexpr;

typedef struct m1_castexpr {
    struct m1_expression *expr;
    int type;    
} m1_castexpr;

typedef enum m1_object_type {
    OBJECT_LINK,  /* node linking a and b in a.b */
    OBJECT_MAIN,  /* a in a.b  */
    OBJECT_FIELD, /* b in a.b  */
    OBJECT_INDEX, /* b in a[b] */
    OBJECT_DEREF, /* b in a->b */
    OBJECT_SCOPE, /* b in a::b */
    OBJECT_SELF,  /* "self"    */
    OBJECT_SUPER  /* "super"   */
    
} m1_object_type;

/* struct to represent an element or link between two elements
   in aggregates. In a.b.c, each element (a, b, c) is represented
   by one m1_object node.
   
 */
typedef struct m1_object {
    
    union {
        char                 *name;  /* for name, field or deref access, in a.b.c for instance. */
        struct m1_expression *index; /* for array index (a[42]) */        
        struct m1_object     *field; /* if this is a linking node (OBJECT_LINK) representing "a.b" as a whole. */
    } obj;
    
    enum m1_object_type type;       /* selector for union */
    struct m1_symbol   *sym;        /* pointer to this object's declaration. */ 
    unsigned            line;       /* line number of symbol that this object is representing. */
    
    struct m1_decl     *decl;       /* pointer to its declaration, if it's a struct. */
    struct m1_object   *parent;     /* pointer to its parent (in a.b.c, a is b's parent) */
      
} m1_object;

typedef enum m1_lhsobj_type {
    LHS_INDEX, /* a[b] */
    LHS_FIELD  /* a.b */
    
} m1_lhsobj_type;

typedef struct m1_lhsobj {
    struct m1_lhsobj *obj;
    struct m1_lhsobj *field;
      
} m1_lhs_obj;

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

/* const declarations */
typedef struct m1_const {
    char                 *type;
    char                 *name;
    struct m1_expression *value;
} m1_const;


/* variable declarations */
typedef struct m1_var {
    char                 *name;
    struct m1_expression *init;
    unsigned              size; /* 1 for non-arrays, larger for arrays */
    struct m1_symbol     *sym;  /* pointer to symbol in symboltable */
    
} m1_var;


typedef struct m1_case {
	int selector;
	struct m1_expression *block;
	
	struct m1_case *next;
	
} m1_case;

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
    struct m0_instr *instr;
    
} m0_block;

/* to represent statements */
typedef struct m1_expression {
    union {
        struct m1_unexpr     *u;
        struct m1_binexpr    *b;
        struct m1_funcall    *f;  
        struct m1_assignment *a; 
        struct m1_whileexpr  *w;  
        struct m1_forexpr    *o;
        struct m1_ifexpr     *i;
        struct m1_expression *e; 
        struct m1_object     *t;
        struct m1_const      *c;
        struct m1_var        *v;
        struct m0_block      *m0;
        struct m1_switch     *s;
        struct m1_newexpr    *n;
        struct m1_literal    *l;
        struct m1_castexpr   *cast;
    } expr;
    
    m1_expr_type  type; /* selector for union */
    unsigned      line; /* line number */
    
    struct m1_expression *next;
    
} m1_expression;


extern m1_chunk *chunk(M1_compiler *comp, char *rettype, char *name, m1_expression *block);

extern m1_expression *expression(M1_compiler *comp, m1_expr_type type);       
extern m1_expression *funcall(M1_compiler *comp, char *name);
            
extern m1_object *object(M1_compiler *comp, m1_object_type type);            
extern void obj_set_ident(m1_object *node, char *ident);

extern m1_structfield *structfield(M1_compiler *comp, char *name, char *type);

extern m1_struct *newstruct(M1_compiler *comp, char *name, m1_structfield *fields);


extern m1_expression *ifexpr(M1_compiler *comp, m1_expression *cond, m1_expression *ifblock, m1_expression *elseblock);
extern m1_expression *whileexpr(M1_compiler *comp, m1_expression *cond, m1_expression *block);
extern m1_expression *dowhileexpr(M1_compiler *comp, m1_expression *cond, m1_expression *block);
extern m1_expression *forexpr(M1_compiler *comp, m1_expression *init, m1_expression *cond, m1_expression *step, m1_expression *stat);

extern m1_expression *inc_or_dec(M1_compiler *comp, m1_expression *obj, m1_unop optype);
extern m1_expression *returnexpr(M1_compiler *comp, m1_expression *retexp);
extern m1_expression *assignexpr(M1_compiler *comp, m1_expression *lhs, int assignop, m1_expression *rhs);
extern m1_expression *objectexpr(M1_compiler *comp, m1_object *obj, m1_expr_type type);

extern m1_expression *binexpr(M1_compiler *comp, m1_expression *e1, m1_binop op, m1_expression *e2);
extern m1_expression *number(M1_compiler *comp, double value);
extern m1_expression *integer(M1_compiler *comp, int value);
extern m1_expression *string(M1_compiler *comp, char *str);
extern m1_expression *unaryexpr(M1_compiler *comp, m1_unop op, m1_expression *e);
extern m1_object *arrayindex(M1_compiler *comp, m1_expression *index);
extern m1_object *objectfield(M1_compiler *comp, char *field);
extern m1_object *objectderef(M1_compiler *comp, char *field);
extern m1_expression *printexpr(M1_compiler *comp, m1_expression *e);
extern m1_expression *constdecl(M1_compiler *comp, char *type, char *name, m1_expression *expr);
extern m1_expression *vardecl(M1_compiler *comp, char *type, m1_var *v);

extern m1_var *var(M1_compiler *comp, char *name, m1_expression *init);
extern m1_var *array(M1_compiler *comp, char *name, unsigned size, m1_expression *init);

extern unsigned field_size(struct m1_structfield *field);

extern m1_expression *switchexpr(M1_compiler *comp, m1_expression *expr, m1_case *cases, m1_expression *defaultstat);
extern m1_case *switchcase(M1_compiler *comp, int selector, m1_expression *block);

extern m1_expression *newexpr(M1_compiler *copm, char *type);

extern m1_object *lhsobj(M1_compiler *comp, m1_object *parent, m1_object *field);
extern m1_expression *castexpr(M1_compiler *comp, int type, m1_expression *castedexpr);


#endif

