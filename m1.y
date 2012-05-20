%{
    
#include <stdio.h>
#include <stdlib.h>
#include "m1lexer.h"
#include "m1_ast.h"
#include "m1_eval.h"

int 
yywrap(void) {
    return 1;
}

/* root of AST */
m1_chunk *ast = NULL;

extern int yyparse(void);

extern char *yytext;
extern int yylineno;
extern int yylex(void);

int 
yyerror(char *str) {
    fprintf(stderr, "%s: unexpected token '%s' (line %d)\n\n", 
            str, yytext, yylineno);
            
    return 0;
}

int
main(int argc, char *argv[]) {
    FILE *fp;
    
    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        fprintf(stderr, "Could not open file\n");
        exit(EXIT_FAILURE);
    }
    yyin = fp;
    yyparse();
    
    eval(ast);
    
    fclose(fp);
    return 0;
}


%}

%union {
    char                    *sval;
    int                      ival;
    double                   fval;
    struct m1_chunk         *chunk;
    struct m1_expression    *expr;
    struct m1_statement     *stat;
    struct m1_funcall       *fun;
    struct m1_object        *obj;
    struct m1_struct        *strct;
    struct m1_structfield   *sfld;
}

%token  TK_IDENT
        TK_NUMBER
        KW_NUM          "num"
        KW_INT          "int"
        TK_INT          
        KW_STRUCT       "struct"
        TK_INC          "++"
        TK_DEC          "--"
        KW_IF           "if"
        KW_WHILE        "while"
        KW_FOR          "for"
        KW_ELSE         "else"
        KW_DO           "do"
        TK_AND          "&&"
        TK_OR           "||"
        TK_ARROW        "->"
        KW_VOID         "void"
        KW_NULL         "null"
        KW_BREAK        "break"
        KW_RETURN       "return"
        KW_CONST        "const"
        TK_GE           ">="
        TK_GT           ">"
        TK_LT           "<"
        TK_LE           "<="
        TK_EQ           "=="
        TK_NE           "!="
        KW_NAMESPACE    "namespace"
        TK_NS_SEP       "::"
        TK_LSH          "<<"
        TK_RSH          ">>"
        
%type <sval> TK_IDENT

%type <chunk> function_definition 
              chunks 
              chunk 
              TOP

%type <ival> return_type 
             type 
             native_type 
             user_type
             TK_INT

%type <fval> TK_NUMBER

%type <expr> expression 
             binexpr 
             inc_or_dec_expr 
             function_call_expr 
             function_call_stat 
             lhs
             rhs
             statement 
             statements 
             block
             inc_or_dec_stat 
             opt_init
             assign_stat
             assign_expr
             if_stat
             while_stat
             do_stat
             for_stat
             arguments
             for_init
             for_cond
             for_step
             return_stat
             break_stat
             tertexpr

%type <sfld> struct_members struct_member
%type <strct> struct_definition
             
%type <obj> field_access             
            lhs_obj
        
%token  KW_M0
        M0_NL   
        M0_IDENT
        M0_NUMBER
        M0_ADD_I
        M0_ADD_N     
        

%defines
%output="m1parser.c"

%start TOP

%left TK_LE TK_GE TK_LT TK_GT TK_EQ TK_NE
%left TK_AND TK_OR TK_LSH TK_RSH
%left '+' '-'
%left '*' '/' '&' '|' '%' '?' ':'
%right '^'
%left TK_INC TK_DEC

/* for dangling else conflict in the grammar; don't
   extend the grammar with many rules to work around, just
   force default (shift) behaviour. This is a documented
   solution in "Lex & Yacc", JR Levine et al., O'Reilly.
*/
%nonassoc LOWER_THAN_ELSE
%nonassoc KW_ELSE

%%

TOP     : chunks
            { 
              $$ = ast = $1; 
            }
        ;
        
chunks  : chunk
            { $$ = $1; }
        | chunks chunk
            { 
              $1->next = $2;
              $$ = $1; 
            }            
        ;
        
chunk   : function_definition            
        | struct_definition
           { $$ = NULL; /* where to store structs? */}
        | namespace_definition
           { $$ = NULL; }
        ;        
        
namespace_definition: "namespace" TK_IDENT ';'
                         { /* store this in the compiler object */ 
                           /* TODO */
                         }        
        
function_definition : return_type TK_IDENT '(' parameters ')' block
                        { $$ = chunk($1, $2, $6); }
                    ;
        
parameters  : /* empty */
            | param_list
            ;
            
param_list  : param
            | param_list ',' param
                { /* TODO */}
            ;            
        
param   : type TK_IDENT
        | type '*' TK_IDENT
        ;
                                             
struct_definition   : "struct" TK_IDENT '{' struct_members '}' ';'
                        { $$ = newstruct($2, $4); }
                    ;         
                    
struct_members      : struct_member
                    | struct_members struct_member
                        { 
                          /* fields are linked in reverse order,
                             but that's ok. 
                          */
                          $$ = $2;
                          $2->next = $1;  
                        }
                          
                    ;
                    
struct_member       : type TK_IDENT ';'
                        { $$ = structfield ($2, $1); }
                    ;                                        
        
block   : '{' statements '}'
            { $$ = $2; }
        ;
        
statements  : /* empty */
                { $$ = NULL; }
            | statements statement
                { 
                    if ($1 == NULL) {
                        $$ = $2;
                        $$->next = NULL;                        
                    }
                    else {
                        m1_expression *iter = $1;
                        while (iter->next != NULL)
                            iter = iter->next;
                            
                        iter->next = $2;
                        $$ = $1;                        
                    }

                }
            ;
            
statement   : assign_stat                
            | if_stat                
            | while_stat                
            | do_stat                
            | for_stat                
            | inc_or_dec_stat                                
            | block              
            | function_call_stat              
            | var_declaration  
                { $$ = NULL; /* TODO */}            
            | const_declaration              
                { $$ = NULL; /* TODO */}
            | return_stat  
            | break_stat   
            | m0_block                                  
                { $$ = NULL; /* TODO */}
            ;
            
m0_block    : KW_M0 '{' m0_instructions '}'
            ;            
            
m0_instructions : m0_instr
                ;
                
m0_instr    : m0_op m0_arg ',' m0_arg ',' m0_arg 
            ;                            
            
m0_arg      : M0_NUMBER
            /* add other argument types for M0 instructions */
            ;
            
m0_op       : M0_ADD_I            
            /* add other M0 ops */
            ;
                  
const_declaration   : "const" type TK_IDENT '=' TK_NUMBER ';'
                    ;                  
                        
var_declaration: type var_list ';'              
               ;         
                              
var_list    : var
            | var_list ',' var
            ;               
            
var         : TK_IDENT opt_init
            ;           
            
opt_init    : /* empty */
                { $$ = NULL; }
            | '=' expression
                { $$ = $2; }
            ;
                        
assign_stat : assign_expr ';'
                { $$ = $1; }
            ;
            
assign_expr : lhs '=' rhs
                { 
                  $$ = expression(EXPR_ASSIGN); 
                  expr_set_assign($$, $1, $3);
                }            
            ;
            
if_stat     : "if" '(' expression ')' statement %prec LOWER_THAN_ELSE 
                { 
                  $$ = expression(EXPR_IF); 
                  expr_set_if($$, $3, $5, NULL);
                }
            | "if" '(' expression ')' statement "else" statement 
                { 
                  $$ = expression(EXPR_IF); 
                  expr_set_if($$, $3, $5, $7);
                }
            ;
            
            
while_stat  : "while" '(' expression ')' statement
                { 
                  $$ = expression(EXPR_WHILE);
                  expr_set_while($$, $3, $5);
                }
            ;
            
do_stat     : "do" block "while" '(' expression ')' ';'
                { 
                  $$ = expression(EXPR_DOWHILE);
                  expr_set_while($$, $5, $2);  
                }
            ;
            
function_call_expr  : TK_IDENT '(' arguments ')' 
                         { 
                           $$ = expression(EXPR_FUNCALL);
                           expr_set_funcall($$, funcall($1)); 
                         }
                    ;
                    
function_call_stat  : function_call_expr ';'
                         { $$ = $1; }
                    ;
                
arguments   : /* empty */
                { $$ = NULL; }
            | expr_list
                { $$ = NULL; }
            ;
            
expr_list   : expression
            | expr_list ',' expression
            ;                                                    
            
for_stat    : "for" '(' for_init ';' for_cond ';' for_step ')' statement
                { 
                   $$ = expression(EXPR_FOR);
                   expr_set_for($$, $3, $5, $7, $9);
                }
            ;  
            
for_init    : /* empty */
                { $$ = NULL; }
            | assign_expr
            ;
            
for_cond    : /* empty */
                { $$ = NULL; }
            | expression
            ;
            
for_step    : /* empty */
                { $$ = NULL; }
            | expression
            ;                                                                      
            

inc_or_dec_expr : lhs "++"
                    { $$ = expression(EXPR_UNARY);
                      expr_set_unexpr($$, $1, UNOP_POSTINC);  
                    }
                | lhs "--"
                    { $$ = expression(EXPR_UNARY);
                      expr_set_unexpr($$, $1, UNOP_POSTDEC);
                    }
                | "++" lhs
                    { $$ = expression(EXPR_UNARY);
                      expr_set_unexpr($$, $2, UNOP_PREINC);
                    }
                | "--" lhs
                    { $$ = expression(EXPR_UNARY);
                      expr_set_unexpr($$, $2, UNOP_PREDEC);
                    }
                ;
                
inc_or_dec_stat : inc_or_dec_expr ';'
                    { $$ = $1; }
                ;

break_stat  : "break" ';'
                { $$ = expression(EXPR_BREAK); }                
                
return_stat : "return" expression ';'
                { 
                  $$ = expression(EXPR_RETURN);
                  expr_set_expr($$, $2);
                }
            ;                
                            
lhs     : lhs_obj
           {
             $$ = expression(EXPR_OBJECT);
             expr_set_obj($$, $1); 
           }
        | '*' lhs_obj
           { 
             $$ = expression(EXPR_DEREF);
             expr_set_obj($$, $2);
           }
        | '&' lhs_obj
           { 
             $$ = expression(EXPR_ADDRESS);
             expr_set_obj($$, $2);             
           }
        ;
        
lhs_obj : TK_IDENT
            { 
              $$ = object(OBJECT_MAIN); 
              obj_set_ident($$, $1);
            }            
        | lhs_obj field_access
            {
              m1_object *iter = $1;
              /* go to end of list to link field access to the end */   
              while (iter->next != NULL)
                iter = iter->next;
              /* found end of list, now link it */  
              iter->next = $2;
              /* always return head of the list */
              $$ = $1;
            }
        ;        
        
field_access: '[' expression ']'
                {
                  $$ = object(OBJECT_INDEX);
                  obj_set_index($$, $2);                    
                }
            | '.' TK_IDENT
                {
                   $$ = object(OBJECT_FIELD);
                   obj_set_ident($$, $2);
                }
            | "->" TK_IDENT
                {
                   $$ = object(OBJECT_DEREF);
                   obj_set_ident($$, $2); 
                }
            ;        

rhs     : expression
        ;
        
        
expression  : TK_NUMBER    
                { 
                  $$ = expression(EXPR_NUMBER); 
                  expr_set_num($$, $1);
                }    
            | TK_INT
                {
                  $$ = expression(EXPR_INT);
                  expr_set_int($$, $1);     
                }  
            | inc_or_dec_expr                
            | '(' expression ')'
                { $$ = $2; }
            | '-' expression
                { 
                  $$ = expression(EXPR_UNARY);
                  expr_set_unexpr($$, $2, UNOP_MINUS);
                }
            | binexpr
            | tertexpr
            | lhs                
            | function_call_expr                
            | "null"
                { $$ = expression(EXPR_NULL); }
            ;
       
tertexpr    : expression '?' expression ':' expression
                { $$ = expression(EXPR_IF); 
                  expr_set_if($$, $1, $3, $5);
                }
            ;
                   
binexpr     : expression '+' expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_PLUS, $3);
                }
            | expression '-' expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_MINUS, $3);
                }
            | expression '*' expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_MUL, $3);
                }
            | expression '/' expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_DIV, $3);
                }
            | expression '%' expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_MOD, $3);
                }
            | expression '^' expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_EXP, $3);
                }
            | expression '&' expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_BAND, $3);
                }
            | expression '|' expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_BOR, $3);
                }
            | expression "==" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_EQ, $3);
                }
            | expression "!=" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_NE, $3);
                }
            | expression ">" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_GT, $3);
                }
            | expression "<" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_LT, $3);
                }
            | expression "<=" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_LE, $3);
                }
            | expression ">=" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_GE, $3);
                }
            | expression "&&" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_AND, $3);
                }
            | expression "||" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_OR, $3);
                }
            | expression "<<" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_LSH, $3);
                }
            | expression ">>" expression
                { $$ = expression(EXPR_BINARY);
                  expr_set_binexpr($$, $1, OP_RSH, $3);
                }
                
            ;
           
return_type : type     { $$ = $1; }
            | "void"  { $$ = TYPE_VOID; }
            ;
            
type    : native_type
        | user_type  
        ;
        
native_type : "int" { $$ = TYPE_INT; }
            | "num" { $$ = TYPE_NUM; }
            ;
            
user_type   : TK_IDENT { $$ = TYPE_USERDEFINED; }
            ;

/* END */
            
%%

/*
grammar specification:

program: chunk*

chunk: function-decl
     | struct-decl
     
function-decl: type NAME ( parameters? ) block

parameters: parameter [, parameter]*

parameter: type NAME

block: { statement* }

statement: if-stat
         | while-stat
         | do-stat
         | assign-stat
         | function-call-stat
         | return-stat
         | block
         | declaration
         | inc-dec-stat
         | for-stat
         
declaration: type NAME [= expression]? [, id [= expression]? ]*
           | const type NAME = VALUE         
           
if-stat: if ( expression ) statement [else statement]?

while-stat: while ( expression ) statement

do-stat: do block while ( expression ) ;            

assign-stat: lhs = rhs ;

function-call: NAME ( arguments? )

function-call-stat: function-call ;

arguments: expression [, expression]*

return-stat: return expression

inc-dec-stat: inc-dec-expression ;

for-stat: for ( [NAME = expression]? ; expression? ; expression? ) statement

lhs: NAME
   | lhs -> NAME
   | lhs [ expression ]
   | lhs . NAME
   | * lhs
   | & lhs

rhs: expression

expression: lhs
          | VALUE
          | function-call
          | expression binop expression
          | unop expression
          | ( expression )
          | inc-dec-expression
          | null
          | expression ? expression : expression
          
inc-dec-expression: ++ lhs
                  | -- lhs
                  | lhs ++
                  | lhs --

type: int
    | num
    | void
    | NAME -- user types 
        
*/
