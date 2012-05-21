%{
    
#include <stdio.h>
#include <stdlib.h>

/* m1parser.h needs to be included /before/ m1lexer.h. */
#include "m1parser.h"

/* prevent declaration of yyparse in m1lexer.h. */
#define YY_DECL
#include "m1lexer.h"

#include "m1_ast.h"
#include "m1_eval.h"
#include "m1_gencode.h"
#include "m1_instr.h"
#include "m1_symtab.h"
#include "m1_compiler.h"


/* Just to make sure that yscan_t can be used as a type in this file.
*/
/*
#ifndef YY_TYPEDEF_YY_SCANNER_T
# define YY_TYPEDEF_YY_SCANNER_T

typedef void * yyscan_t;

#endif
*/

#ifndef YYLTYPE_IS_TRIVIAL
# define YYLTYPE_IS_TRIVIAL 0
#endif

extern m0_instr *instr(char op, char arg1, char arg2, char arg3);
extern int yyparse(yyscan_t yyscanner, struct M1_compiler * const comp);
extern int yylex(YYSTYPE *yylval, yyscan_t yyscanner);

int 
yyerror(yyscan_t yyscanner, M1_compiler *comp, char *str) {

    fprintf(stderr, "%s: unexpected token '%s' (line %d)\n\n", 
            str, yyget_text(yyscanner), yyget_lineno(yyscanner) );
           
    return 0;
}

int
main(int argc, char *argv[]) {
    FILE        *fp;
    yyscan_t     yyscanner;
    M1_compiler  comp;
    
    fp = fopen(argv[1], "r");
    if (fp == NULL) {
        fprintf(stderr, "Could not open file\n");
        exit(EXIT_FAILURE);
    }
   
   
    yylex_init(&yyscanner);
    yyset_extra(&comp, yyscanner);
    
    yyset_in(fp, yyscanner);

    init_symtabs();
    yyparse(yyscanner, &comp);
    
    gencode(&comp, comp.ast);
    
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
    struct m1_object        *obj;
    struct m1_struct        *strct;
    struct m1_structfield   *sfld;
    struct m1_var           *var;
    struct m0_instr         *instr;
}




%token  TK_IDENT
        TK_NUMBER
        KW_NUM          "num"
        KW_INT          "int"
        KW_STRING       "string"
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
        TK_STRING_CONST
        TK_INC_ASSIGN   "+="
        TK_DEC_ASSIGN   "-="
        KW_CASE         "case"
        KW_DEFAULT      "default"
        KW_SWITCH       "switch"
        KW_PRINT        "print"
        
%type <sval> TK_IDENT
             TK_STRING_CONST

%type <chunk> function_definition 
              chunks 
              chunk 
              TOP

%type <ival> return_type 
             type 
             native_type 
             user_type
             TK_INT
             m0_op
             m0_arg

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
             constexpr
             switch_stat
             const_declaration
             var_declaration
             unexpr
             m0_block
             print_stat
             
%type <instr> m0_instructions
              m0_instr
       

%type <var>  var
%type <sfld> struct_members struct_member
%type <strct> struct_definition
             
%type <obj> field_access             
            lhs_obj
        

        
%token  KW_M0		"M0"
        TK_NL   
        M0_NUMBER
        KW_ADD_I	"add_i"
        KW_ADD_N    "add_n"
        
%pure-parser

%parse-param	{yyscan_t yyscanner}
%lex-param		{yyscan_t yyscanner}
%parse-param	{struct M1_compiler * const comp}

%defines
%output="m1parser.c"

%start TOP

%nonassoc TK_INC_ASSIGN
%left TK_LE TK_GE TK_LT TK_GT TK_EQ TK_NE
%left TK_AND TK_OR TK_LSH TK_RSH
%left '+' '-'
%left '*' '/' '&' '|' '%' '?' ':' '!'
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
              M1_compiler *comp = yyget_extra(yyscanner);
              comp->ast = $1; 
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
            | const_declaration                           
            | return_stat  
            | break_stat  
            | switch_stat 
            | print_stat
            | m0_block                                      
            ;
            
print_stat  : "print" '(' expression ')' ';'
                { $$ = printexpr($3); }
            ;
                        
m0_block    : "M0" '{' m0_instructions '}'
                { $$ = expression(EXPR_M0BLOCK); }
            ;            
            
m0_instructions : m0_instr

                | m0_instructions m0_instr
                    { 
                      $1->next = $2; 
                      $$ = $1;
                    }
                ;
                
m0_instr    : m0_op m0_arg ',' m0_arg ',' m0_arg
                { $$ = instr($1, $2, $4, $6); }
            | m0_op m0_arg ',' m0_arg ',' 'x'
                { $$ = instr($1, $2, $4, 0); }
            | m0_op m0_arg ',' 'x' ',' 'x'
                { $$ = instr($1, $2, 0, 0); }
            | m0_op 'x' ',' 'x' ',' 'x'
                { $$ = instr($1, 0, 0, 0); }
            ;                            
            
m0_arg      : M0_NUMBER  {$$=0;}
            /* add other argument types for M0 instructions */
            ;
            
m0_op       : KW_ADD_I   {$$=0;}         
            /* add other M0 ops */
            ;
                  
const_declaration   : "const" type TK_IDENT '=' constexpr ';'
                        { $$ = constdecl($2, $3, $5); }
                    ;                  
                        
var_declaration: type var ';'  
                    { $$ = vardecl($1, $2); }            
               ;         
/*                              
var_list    : var
            | var_list ',' var
            ;               
*/            
var         : TK_IDENT opt_init
                { $$ = var($1); }
            | TK_IDENT '[' TK_INT ']'
                { $$ = var($1); }
            ;           
            
opt_init    : /* empty */
                { $$ = NULL; }
            | '=' expression
                { $$ = $2; }
            ;
                        
assign_stat : assign_expr ';'
                { $$ = $1; }
            ;
            
assign_expr : lhs assignop rhs
                { $$ = assignexpr($1, $3); }
            
            ;
            
assignop    : '='
            | "+="
            | "-="
            ;            
            
if_stat     : "if" '(' expression ')' statement %prec LOWER_THAN_ELSE 
                { $$ = ifexpr($3, $5, NULL); }
            | "if" '(' expression ')' statement "else" statement 
                { $$ = ifexpr($3, $5, $7); }
            ;
            
            
while_stat  : "while" '(' expression ')' statement
                { $$ = whileexpr($3, $5); }
            ;
            
do_stat     : "do" block "while" '(' expression ')' ';'
                { $$ = dowhileexpr($5, $2); }
            ;
            
switch_stat : "switch" '(' expression ')' '{' cases default_case '}'
                { $$ = NULL; }
            ;
            
cases       : /* empty */
            | cases case 
            ;
            
case        : "case" TK_INT ':' statements                       
            ;
            
default_case: /* empty */
            | "default" ':' statements
            ;
                        
function_call_expr  : TK_IDENT '(' arguments ')' 
                         { $$ = funcall($1); }
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
                { $$ = forexpr($3, $5, $7, $9); }
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
                    { $$ = inc_or_dec($1, UNOP_POSTINC); }
                | lhs "--"
                    { $$ = inc_or_dec($1, UNOP_POSTDEC); }
                | "++" lhs
                    { $$ = inc_or_dec($2, UNOP_PREINC); }
                | "--" lhs
                    { $$ = inc_or_dec($2, UNOP_PREDEC); }                    
                ;
                
inc_or_dec_stat : inc_or_dec_expr ';'
                    { $$ = $1; }
                ;

break_stat  : "break" ';'
                { $$ = expression(EXPR_BREAK); }                
                
return_stat : "return" expression ';'
                { $$ = returnexpr($2); }
            ;                
                            
lhs     : lhs_obj
           { $$ = objectexpr($1, EXPR_OBJECT); }           
        | '*' lhs_obj
           { $$ = objectexpr($2, EXPR_DEREF); }
        | '&' lhs_obj
           { $$ = objectexpr($2, EXPR_ADDRESS); }
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
                { $$ = arrayindex($2); }                
            | '.' TK_IDENT
                { $$ = objectfield($2); }           
            | "->" TK_IDENT
                { $$ = objectderef($2); }                
            ;        

rhs     : expression
        ;
        
constexpr   : TK_NUMBER    
                { $$ = number($1); }    
            | TK_INT
                { $$ = integer($1); }  
            | TK_STRING_CONST
                { $$ = string($1); }
            ;
            
expression  : constexpr 
            | inc_or_dec_expr                
            | '(' expression ')'
                { $$ = $2; }
            | unexpr             
            | binexpr
            | tertexpr
            | lhs                
            | function_call_expr                
            | "null"
                { $$ = expression(EXPR_NULL); }
            ;
            
unexpr  : '-' expression
                { $$ = unaryexpr(UNOP_MINUS, $2); }                
        | '!' expression
                { $$ = unaryexpr(UNOP_NOT, $2); }                            
        ;            
       
tertexpr    : expression '?' expression ':' expression
                { $$ = ifexpr($1, $3, $5); }
            ;
                   
binexpr     : expression '+' expression
                { $$ = binexpr($1, OP_PLUS, $3); }
            | expression '-' expression
                { $$ = binexpr($1, OP_MINUS, $3); }
            | expression '*' expression
                { $$ = binexpr($1, OP_MUL, $3); }
            | expression '/' expression
                { $$ = binexpr($1, OP_DIV, $3); }
            | expression '%' expression
                { $$ = binexpr($1, OP_MOD, $3); }
            | expression '^' expression
                { $$ = binexpr($1, OP_EXP, $3); }
            | expression '&' expression
                { $$ = binexpr($1, OP_BAND, $3); }
            | expression '|' expression
                { $$ = binexpr($1, OP_BOR, $3); }
            | expression "==" expression
                { $$ = binexpr($1, OP_EQ, $3); }
            | expression "!=" expression
                { $$ = binexpr($1, OP_NE, $3); }
            | expression ">" expression
                { $$ = binexpr($1, OP_GT, $3); }
            | expression "<" expression
                { $$ = binexpr($1, OP_LT, $3); }
            | expression "<=" expression
                { $$ = binexpr($1, OP_LE, $3); }
            | expression ">=" expression
                { $$ = binexpr($1, OP_GE, $3); }
            | expression "&&" expression
                { $$ = binexpr($1, OP_AND, $3); }
            | expression "||" expression
                { $$ = binexpr($1, OP_OR, $3); }
            | expression "<<" expression
                { $$ = binexpr($1, OP_LSH, $3); }
            | expression ">>" expression
                { $$ = binexpr($1, OP_RSH, $3); }
                
            ;
           
return_type : type     { $$ = $1; }
            | "void"  { $$ = TYPE_VOID; }
            ;
            
type    : native_type
        | user_type  
        ;
        
native_type : "int"     { $$ = TYPE_INT; }
            | "num"     { $$ = TYPE_NUM; }
            | "string"  { $$ = TYPE_STRING; }
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
