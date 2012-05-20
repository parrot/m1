#ifndef __M1_AST_H__
#define __M1_AST_H__

#include "m1_instr.h"

typedef enum data_types {
    TYPE_INT    = 0,
    TYPE_NUM    = 1,
    TYPE_STRING = 2,
    TYPE_PMC    = 3,
    TYPE_VOID   = 4,
    TYPE_USERDEFINED = 5
    
} data_type;

typedef struct m1_chunk {
    data_type rettype;
    char *name;
    struct m1_chunk *next;
    struct m1_expression *block;
    /* TODO: add parameters */
        
} m1_chunk;

typedef struct m1_structfield {
    char     *name;
    data_type type;    
    struct m1_structfield *next;
    
} m1_structfield;

typedef struct m1_struct {
    char    *name;
    unsigned numfields;
    struct m1_structfield *fields;
    
} m1_struct;


typedef struct m1_assignment {
    struct m1_expression *lhs;
    struct m1_expression *rhs;
    
} m1_assignment;

typedef struct m1_funcall {
    char *name;
    /* TODO: add args */
    
} m1_funcall;



typedef enum m1_expr_type {
    EXPR_NUMBER,
    EXPR_INT,
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_FUNCALL,
    EXPR_ASSIGN,
    EXPR_IF,
    EXPR_WHILE,
    EXPR_DOWHILE,
    EXPR_FOR,
    EXPR_RETURN,
    EXPR_NULL,
    EXPR_DEREF,
    EXPR_ADDRESS,
    EXPR_OBJECT,
    EXPR_BREAK,
    EXPR_STRING,
    EXPR_CONSTDECL,
    EXPR_VARDECL,
    EXPR_M0BLOCK,
    EXPR_PRINT
} m1_expr_type;


typedef enum m1_binop {
    OP_PLUS,
    OP_MINUS,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_EXP,
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
    UNOP_MINUS,    /* -a */ 
    UNOP_NOT
} m1_unop;

typedef struct m1_unexpr {
    struct m1_expression *expr;
    m1_unop op;
    
} m1_unexpr;

typedef enum m1_object_type {
    OBJECT_MAIN,  /* a in a.b  */
    OBJECT_FIELD, /* b in a.b  */
    OBJECT_INDEX, /* b in a[b] */
    OBJECT_DEREF  /* b in a->b */
} m1_object_type;

typedef struct m1_object {
    
    union {
        char *field;  /* for name, field or deref access */
        struct m1_expression *index; /* for array index */        
    } obj;
    
    enum m1_object_type type;
    
    struct m1_object *next;  
      
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

typedef struct m1_const {
    data_type type;
    char *name;
    struct m1_expression *value;
} m1_const;

typedef struct m1_var {
    data_type type;
    char *name;
    
} m1_var;




typedef struct m0_block {
    struct m0_instr *instr;
    
} m0_block;

/* to represent statements */
typedef struct m1_expression {
    union {
        struct m1_unexpr     *u;
        struct m1_binexpr    *b;
        double               floatval;
        int                  intval;
        char                 *str;   
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
    } expr;
    
    m1_expr_type      type;
    struct m1_symbol *sym;
    
    struct m1_expression *next;
    
} m1_expression;


extern m1_chunk *chunk(int rettype, char *name, m1_expression *block);



extern m1_expression *expression(m1_expr_type type);
extern void expr_set_num(m1_expression *e, double v);
extern void expr_set_int(m1_expression *e, int v);

extern void expr_set_binexpr(m1_expression *node, 
                             m1_expression *e1, 
                             m1_binop op, 
                             m1_expression *e2);

extern void expr_set_unexpr(m1_expression *node, m1_expression *exp, m1_unop op);             
       
extern m1_funcall *funcall(char *name);
extern void expr_set_funcall(m1_expression *node, m1_funcall *f);
extern void expr_set_for(m1_expression *node, 
                         m1_expression *init,
                         m1_expression *cond,
                         m1_expression *step,
                         m1_expression *stat);

extern void expr_set_while(m1_expression *node,
                           m1_expression *cond,
                           m1_expression *block);


extern void expr_set_expr(m1_expression *node, m1_expression *expr);
extern void expr_set_obj(m1_expression *node, m1_object *obj);

extern void expr_set_assign(m1_expression *node, 
                            m1_expression *lhs, m1_expression *rhs);

extern void obj_set_ident(m1_object *node, char *ident);
extern void obj_set_index(m1_object *node, m1_expression *index);

extern void expr_set_if(m1_expression *node, m1_expression *cond, 
            m1_expression *ifblock, m1_expression *elseblock);
            
extern m1_object *object(m1_object_type type);            

extern m1_structfield * structfield(char *name, data_type type);

extern m1_struct *newstruct(char *name, m1_structfield *fields);

extern void expr_set_string(m1_expression *node, char *str);


extern void expr_set_const_decl(m1_expression *node, data_type type, 
                    char *name, m1_expression *expr); 

extern void expr_set_var_decl(m1_expression *node, data_type type, m1_var *decl);

extern m1_var *var(char *name);
#endif

