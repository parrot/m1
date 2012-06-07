%{
    
#include <stdio.h>
#include <stdlib.h>

/* m1parser.h needs to be included /before/ m1lexer.h. */
#include "m1parser.h"

/* prevent declaration of yyparse in m1lexer.h. */
#define YY_DECL
#include "m1lexer.h"

#include "ast.h"
#include "instr.h"
#include "compiler.h"
#include "decl.h"



extern int yylex(YYSTYPE *yylval, yyscan_t yyscanner);



int 
yyerror(yyscan_t yyscanner, M1_compiler *comp, char *str) {

    fprintf(stderr, "%s: unexpected token '%s' (line %d)\n\n", 
            str, yyget_text(yyscanner), yyget_lineno(yyscanner) );
    ++comp->errors;        
    return 0;
}




%}

%union {
    char                     cval;
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
    struct m1_case			*cse;
}




%token  TK_IDENT
        TK_NUMBER
        TK_CHAR
        KW_NUM          "num"
        KW_INT          "int"
        KW_STRING       "string"
        KW_CHAR         "char"
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
        KW_CONTINUE     "continue"
        KW_RETURN       "return"
        KW_CONST        "const"
        TK_GE           ">="
        TK_GT           ">"
        TK_LT           "<"
        TK_LE           "<="
        TK_EQ           "=="
        TK_NE           "!="
        KW_NAMESPACE    "namespace"
        TK_SCOPE        "::"
        TK_LSH          "<<"
        TK_RSH          ">>"
        TK_STRING_CONST
        TK_INC_ASSIGN   "+="
        TK_DEC_ASSIGN   "-="
        TK_MUL_ASSIGN   "*="
        TK_DIV_ASSIGN   "/="
        TK_MOD_ASSIGN   "%="
        TK_SHL_ASSIGN   ">>="
        TK_SHR_ASSIGN   "<<="
        TK_BAND_ASSIGN  "&="
        TK_BOR_ASSIGN   "|="
        KW_CASE         "case"
        KW_DEFAULT      "default"
        KW_SWITCH       "switch"
        KW_PRINT        "print"
        KW_PMC			"pmc"
        KW_EXTENDS		"extends"
        KW_VTABLE		"vtable"
        KW_METHOD		"method"
        KW_NEW			"new"
        KW_SUPER		"super"
        KW_SELF			"self"
        KW_FALSE        "false"
        KW_TRUE         "true"
        TK_ISTRUE       "?"
        TK_NOT          "!"
        KW_EXTERN       "extern"
        KW_IMPORT       "import"
        KW_UNSIGNED     "unsigned"
        KW_BOOL         "bool"
        KW_CATCH        "catch"
        KW_THROW        "throw"
        KW_TRY          "try"
        KW_INLINE       "inline"
        KW_PRIVATE      "private"
        KW_PUBLIC       "public"
        KW_ENUM         "enum"

        
%type <sval> TK_IDENT
             TK_STRING_CONST
             return_type 
             type 
             native_type 
             TK_USERTYPE
             
%type <chunk> function_definition 
              function_init
              chunks 
              chunk 
              TOP
             
%type <ival> TK_INT
             m0_op
             m0_arg                       
             
%type <ival> assignop
             
%type <cval> TK_CHAR

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
             continue_stat
             tertexpr
             constexpr
             switch_stat
             const_declaration
             var_declaration
             unexpr
             m0_block
             print_stat
             default_case
             arrayconstructor
             nullexpr
             subexpr
             newexpr
             opt_array_init
             opt_ret_expr
             
%type <instr> m0_instructions
              m0_instr
       

%type <var>  var
             var_list
%type <sfld> struct_members 
             struct_member
%type <strct> struct_definition
             
%type <obj> field_access             
            lhs_obj
        
%type <cse> case 
            cases

        
%token  KW_M0		    "M0"
        TK_NL   
        M0_INT_CONST
        KW_NOOP         "noop"
        KW_GOTO         "goto"
        KW_GOTO_IF      "goto_if"
        KW_GOTO_CHUNK   "goto_chunk"
        KW_ADD_I        "add_i"
        KW_ADD_N        "add_n"
        KW_SUB_I        "sub_i"
        KW_SUB_N        "sub_n"
        KW_MULT_I       "mult_i"
        KW_MULT_N       "mult_n"
        KW_DIV_I        "div_i"
        KW_DIV_N        "div_n"
        KW_MOD_I        "mod_i"
        KW_MOD_N        "mod_n"
        KW_ITON         "iton"
        KW_NTOI         "ntoi"
        KW_ASHR         "ashr"
        KW_LSHR         "lshr"
        KW_SHL          "shl"
        KW_AND          "and"
        KW_OR           "or"
        KW_XOR          "xor"
        KW_GC_ALLOC     "gc_alloc"
        KW_SYS_ALLOC    "sys_alloc"
        KW_SYS_FREE     "sys_free"
        KW_COPY_MEM     "copy_mem"
        KW_SET          "set"
        KW_SET_IMM      "set_imm"
        KW_DEREF        "deref"
        KW_SET_REF      "set_ref"
        KW_SET_BYTE     "set_byte"
        KW_GET_BYTE     "get_byte"
        KW_SET_WORD     "set_word"
        KW_GET_WORD     "get_word"
        KW_CSYM         "csym"
        KW_CCALL_ARG    "ccall_arg"
        KW_CCALL_RET    "ccall_ret"
        KW_CCALL        "ccall"
        KW_PRINT_S      "print_s"
        KW_PRINT_I      "print_i"
        KW_PRINT_N      "print_n"
        KW_EXIT         "exit"
        TK_USERTYPE

        
%pure-parser

%parse-param	{yyscan_t yyscanner}
%lex-param		{yyscan_t yyscanner}
%parse-param	{struct M1_compiler * const comp}

%defines
%expect 0

%start TOP


%left  ':' 
%nonassoc TK_INC_ASSIGN '='
%left TK_AND TK_OR 
%left TK_LE TK_GE TK_LT TK_GT TK_EQ TK_NE
%left TK_LSH TK_RSH
%left '+' '-' 
%left '*' '/' '&' '|' '%'  '^'
%left TK_INC TK_DEC 
%left TK_NOT

/* for dangling else conflict in the grammar; don't
   extend the grammar with many rules to work around, just
   force default (shift) behaviour. This is a documented
   solution in "Lex & Yacc", JR Levine et al., O'Reilly.
   
*/
%nonassoc LOWER_THAN_ELSE
%nonassoc KW_ELSE TK_ISTRUE 


%%

TOP     : imports chunks
            { 
              M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);
              comp->ast = $2; 
            }
        ;
        
imports : /* empty */
        | imports importstat
        ;
        
importstat  : "import" TK_IDENT ';' 
                { fprintf(stderr, "'import' statement not implemented yet!\n"); }      
            ;
        
chunks  : chunk
            { $$ = $1; }
        | chunks chunk
            { 
              if ($1 != NULL) {
                m1_chunk *iter = $1;
                while (iter->next != NULL)
                    iter = iter->next;
                iter->next = $2;
                $$ = $1; 
              }
              else {
                $$ = $2; 
              }

              
            }            
        ;
        
chunk   : function_definition            
        | struct_definition
           { $$ = NULL; }
        | namespace_definition
           { $$ = NULL; /* do we want namespaces? */ 
           fprintf(stderr, "namespaces are not implemented yet!\n");
           }
        | pmc_definition
           { $$ = NULL; /* TODO */ 
           fprintf(stderr, "PMC definitions are not implemented yet!\n");
           }
        | enum_definition
           { $$ = NULL; /* TODO */
           fprintf(stderr, "enum definitions are not implemented yet!\n");
           }
        ;        


enum_definition : "enum" TK_IDENT '{' enum_constants '}' ';' 
                    { }
                ;
        
enum_constants  : enum_const 
                    {  }
                | enum_constants ',' enum_const 
                    { }
                ; 
                  
enum_const      : TK_IDENT opt_enum_val
                ;
                
opt_enum_val    : 
                |'=' TK_INT
                ;
                   
namespace_definition: "namespace" TK_IDENT ';'
                         { 
                           /* TODO */
                         }    
                     ;    

/* TODO: PMC handling */
pmc_definition	: "pmc" TK_IDENT extends_clause '{'  pmc_items '}'
                ;
                                
extends_clause	: /* empty */
                | "extends" id_list
                ;
                
id_list			: TK_IDENT
                | id_list ',' TK_IDENT
                ;                

pmc_items		: /* empty */
                | pmc_items pmc_item
                ;
                
pmc_item		: pmc_attr
				| pmc_method
				;

pmc_attr		: var_declaration
                ;

pmc_method		: opt_vtable "method" TK_IDENT '(' parameters ')' block 
                    {}
				;
				
opt_vtable      : /* empty */
                | "vtable"
                ;				
                                        
function_definition : function_init '(' parameters ')' block
                        { 
                          $1->block = $5;  
                          print_symboltable(&comp->currentchunk->locals);
                          $$ = $1;
                        }
                    ;

function_init   : return_type TK_IDENT
                        {
                          M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);                          
                          $$ = chunk(comp, $1, $2, NULL); 
                          comp->currentchunk = $$;
                          /* enter name of function declaration in table */
                          sym_enter_chunk(&comp->currentchunk->constants, $2);
                        }
                ;

/* TODO: parameter handling. */        
parameters  : /* empty */
            | param_list
            ;
            
param_list  : param
            | param_list ',' param
                { /* TODO */ fprintf(stderr, "parameters are not implemented yet!\n"); }
            ;            
        
param   : type TK_IDENT
        | type '*' TK_IDENT
        ;
                                             
struct_definition   : "struct" TK_IDENT '{' struct_members '}' 
                        { 
                          M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);
                          $$ = newstruct(comp, $2, $4); 
                          type_enter_struct(comp, $2, $$);
                        }
                    ;         
                    
struct_members      : struct_member
                        { 
                          $1->offset = 0; /* first field, no offset */
                          $$ = $1;
                        }
                    | struct_members struct_member
                        { 
                          /* fields are linked in reverse order, but that's ok. */
                          
                          /* calculate offset of this field */
                          $2->offset = $1->offset + field_size($1);
                           
                          $$ = $2;
                          $2->next = $1;                            
                        }
                          
                    ;
                    
struct_member       : return_type TK_IDENT ';'
                        { 
                           M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);
                           $$ = structfield(comp, $2, $1); 
                        }
                    ;                                        
        
block   : open_block statements close_block
            { $$ = $2; }
        ;
        
open_block: '{'   
                  { /* open new scope, increment scope counter. */
                    M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner); 
                    comp->currentscope++;
                  }

close_block: '}'  
                  { /* closing current scope, decrement scope counter. */
                    M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner); 
                    comp->currentscope--;
                  }
        
statements  : /* empty */
                { $$ = NULL; }
            | statements statement
                { 
                    if ($1 == NULL) {
                        $$ = $2;
                        $$->next = NULL;                        
                    }
                    else { /* add $2 to end of list. */
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
            | continue_stat 
            | switch_stat 
            | print_stat
            | try_stat { $$ = NULL; fprintf(stderr, "try stat not implemented!\n"); }
            | throw_stat { $$ = NULL; fprintf(stderr, "throw stat not implemented!\n"); }
            | m0_block                                      
            ;

try_stat    : "try" block catch_blocks
            ;
            
catch_blocks: catch_block
            | catch_blocks catch_block
            ;
            
catch_block : "catch" '(' TK_IDENT ')' block
            ;

throw_stat  : "throw" expression ';'
            ;
                                    
print_stat  : "print" '(' expression ')' ';'
                { $$ = printexpr((M1_compiler *)yyget_extra(yyscanner), $3); }
            ;

                  
const_declaration   : "const" type TK_IDENT '=' constexpr ';'
                        { $$ = constdecl((M1_compiler *)yyget_extra(yyscanner), $2, $3, $5); }
                    ;                  
                        
var_declaration: type var_list ';'  
                    { $$ = vardecl((M1_compiler *)yyget_extra(yyscanner), $1, $2); }            
               ;         
                              
var_list    : var 				
               { $$ = $1; }
            | var_list ',' var	
               { 
                  /* link nodes in reverse order, but that's ok. */
                  $3->next = $1;                   
                  $$ = $3;               
               }
            
            ;               
            
var         : TK_IDENT opt_init
                { $$ = var((M1_compiler *)yyget_extra(yyscanner), $1, $2); }
            | TK_IDENT '[' TK_INT ']' opt_array_init
                { $$ = array((M1_compiler *)yyget_extra(yyscanner), $1, $3, $5); }
            ;           
            
opt_array_init  : /* empty */
                    { $$ = NULL; }
                | '=' arrayconstructor  
                    { $$ = $2; }    
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
                { 

                    $$ = assignexpr((M1_compiler *)yyget_extra(yyscanner), $1, $2, $3); 
                 }            
            ;
            
assignop    : '='   { $$ = OP_ASSIGN; }
            | "+="  { $$ = OP_PLUS; }
            | "-="  { $$ = OP_MINUS; }
            | "*="  { $$ = OP_MUL; }
            | "/="  { $$ = OP_DIV; }
            | "%="  { $$ = OP_MOD; }
            | ">>=" { $$ = OP_RSH; }
            | "<<=" { $$ = OP_LSH; }
            | "|="  { $$ = OP_BOR; }
            | "&="  { $$ = OP_BAND; }
            ;            
            
           
if_stat     : "if" '(' expression ')' statement %prec LOWER_THAN_ELSE  
                { $$ = ifexpr((M1_compiler *)yyget_extra(yyscanner), $3, $5, NULL); }
            | "if" '(' expression ')' statement "else" statement 
                { $$ = ifexpr((M1_compiler *)yyget_extra(yyscanner), $3, $5, $7); }
            ;
            
            
while_stat  : "while" '(' expression ')' statement 
                { $$ = whileexpr((M1_compiler *)yyget_extra(yyscanner), $3, $5); }
            ;
            
do_stat     : "do" block "while" '(' expression ')' ';' 
                { $$ = dowhileexpr((M1_compiler *)yyget_extra(yyscanner), $5, $2); }
            ;
            
switch_stat : "switch" '(' expression ')' '{' cases default_case '}'
                { $$ = switchexpr((M1_compiler *)yyget_extra(yyscanner), $3, $6, $7); }
            ;
            
cases       : /* empty */
				{ $$ = NULL; }
            | cases case 
            	{   /* link them in reverse order as order doesn't matter. */
            		$2->next = $1;
            		$$ = $2; 
            	}
            ;
            
case        : "case" TK_INT ':' statements                       
				{ $$ = switchcase((M1_compiler *)yyget_extra(yyscanner), $2, $4); }
            ;
            
default_case: /* empty */
				{ $$ = NULL; }
            | "default" ':' statements
            	{ $$ = $3; }
            ;
             
/* TODO: at some point we want x.y(); replace TK_IDENT with "lhs". Get this working first though*/           
function_call_expr  : TK_IDENT '(' arguments ')' 
                         { $$ = funcall((M1_compiler *)yyget_extra(yyscanner), $1); }
                    ;
                    
function_call_stat  : function_call_expr ';'
                         { $$ = $1; }
                    ;

/* TODO: argument handling  */                
arguments   : /* empty */
                { $$ = NULL; fprintf(stderr, "no arguments\n"); }
            | expr_list
                { $$ = NULL; }
            ;
            
expr_list   : expression 
            | expr_list ',' expression /* TODO */
            ;                                                    
            
for_stat    : "for" '(' for_init ';' for_cond ';' for_step ')' statement
                { $$ = forexpr((M1_compiler *)yyget_extra(yyscanner), $3, $5, $7, $9); }
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
                    { $$ = inc_or_dec((M1_compiler *)yyget_extra(yyscanner), $1, UNOP_POSTINC); }
                | lhs "--"
                    { $$ = inc_or_dec((M1_compiler *)yyget_extra(yyscanner), $1, UNOP_POSTDEC); }
                | "++" lhs
                    { $$ = inc_or_dec((M1_compiler *)yyget_extra(yyscanner), $2, UNOP_PREINC); }
                | "--" lhs
                    { $$ = inc_or_dec((M1_compiler *)yyget_extra(yyscanner), $2, UNOP_PREDEC); }                    
                ;
                
inc_or_dec_stat : inc_or_dec_expr ';'
                    { $$ = $1; }
                ;

break_stat  : "break" ';'
                { $$ = expression((M1_compiler *)yyget_extra(yyscanner), EXPR_BREAK); }                
continue_stat  : "continue" ';'
                { $$ = expression((M1_compiler *)yyget_extra(yyscanner), EXPR_CONTINUE); }                
                
return_stat : "return" opt_ret_expr ';'
                { $$ = returnexpr((M1_compiler *)yyget_extra(yyscanner), $2); }
            ;   
            
opt_ret_expr: /* empty */     { $$ = NULL; }
            | expression      { $$ = $1; }
            ;                         
                            
lhs     : lhs_obj
           { $$ = objectexpr((M1_compiler *)yyget_extra(yyscanner), $1, EXPR_OBJECT); }           
        | '*' lhs_obj
           { $$ = objectexpr((M1_compiler *)yyget_extra(yyscanner), $2, EXPR_DEREF); }
        | '&' lhs_obj
           { $$ = objectexpr((M1_compiler *)yyget_extra(yyscanner), $2, EXPR_ADDRESS); }
        ;
        
lhs_obj : TK_IDENT
            { 
              M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);
              /* look up identifier's declaration. */
              m1_symbol *sym    = sym_lookup_symbol(&comp->currentchunk->locals, $1, comp->currentscope);
              
              if (sym == NULL) {
                fprintf(stderr, "Undeclared variable: ");
                yyerror(yyscanner, comp, $1); 
              }
              
              $$ = object(comp, OBJECT_MAIN); 
              obj_set_ident($$, $1);
              $$->sym = sym;      
            }            
        | lhs_obj field_access
            {
              /* make a new node that links $1 and $2. */
              $$ = lhsobj((M1_compiler *)yyget_extra(yyscanner), $1, $2);                                          
            }
        | "self"
        	{ $$ = object((M1_compiler *)yyget_extra(yyscanner), OBJECT_SELF); }
        | "super"
        	{ $$ = object((M1_compiler *)yyget_extra(yyscanner), OBJECT_SUPER); }
        ;        
        
field_access: '[' expression ']'
                { $$ = arrayindex((M1_compiler *)yyget_extra(yyscanner), $2); }                
            | '.' TK_IDENT
                { $$ = objectfield((M1_compiler *)yyget_extra(yyscanner), $2); }           
            | "->" TK_IDENT
                { $$ = objectderef((M1_compiler *)yyget_extra(yyscanner), $2); }                
            | "::" TK_IDENT
                { $$ = NULL; /* do we want this scope operator? */}
            ;        

rhs     : expression
        ;
        
constexpr   : TK_NUMBER    
                { $$ = number((M1_compiler *)yyget_extra(yyscanner), $1); }    
            | TK_INT
                { $$ = integer((M1_compiler *)yyget_extra(yyscanner), $1); }  
            | TK_STRING_CONST
                { $$ = string((M1_compiler *)yyget_extra(yyscanner), $1); }
            | "true"
                { $$ = expression((M1_compiler *)yyget_extra(yyscanner), EXPR_TRUE); }
            | "false"
                { $$ = expression((M1_compiler *)yyget_extra(yyscanner), EXPR_FALSE); }
            | TK_CHAR
                { $$ = character((M1_compiler *)yyget_extra(yyscanner), $1); } 
            ;
            
expression  : constexpr 
            | inc_or_dec_expr                
            | subexpr
            | unexpr             
            | binexpr
            | tertexpr
            | lhs                
            | function_call_expr                
            | nullexpr           
            | newexpr
            | arrayconstructor
            ;
            
subexpr     : '(' expression ')'
                { $$ = $2; }           
            ;
            

            
newexpr     : "new" TK_USERTYPE '(' arguments ')'
                { $$ = newexpr((M1_compiler *)yyget_extra(yyscanner), $2); }
            ;         
            
nullexpr    : "null"
                { $$ = expression((M1_compiler *)yyget_extra(yyscanner), EXPR_NULL); }            
            ;
            
arrayconstructor: '{' const_list '}' 
                     { $$ = NULL; }
                ;  
            
                           
const_list    : constexpr
              | const_list ',' constexpr            
              ;
            
unexpr  : '-' expression
               { 
                 M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);
                 $$ = binexpr(comp, $2, OP_MUL, integer(comp, -1)); }                                          
        | '(' return_type ')' expression %prec LOWER_THAN_ELSE
                { $$ = castexpr((M1_compiler *)yyget_extra(yyscanner), $2, $4); }
        | "!" expression 
                { $$ = unaryexpr((M1_compiler *)yyget_extra(yyscanner), UNOP_NOT, $2); }                        
        ;            
       
tertexpr    : expression "?" expression ':' expression
                { $$ = ifexpr((M1_compiler *)yyget_extra(yyscanner), $1, $3, $5); }
            ;
                   
binexpr     : expression '=' expression /* to allow writing: a = b = c; */
				{ $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_ASSIGN, $3); }
            | expression '+' expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_PLUS, $3); }
            | expression '-' expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_MINUS, $3); }
            | expression '*' expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_MUL, $3); }
            | expression '/' expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_DIV, $3); }
            | expression '%' expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_MOD, $3); }
            | expression '^' expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_XOR, $3); }
            | expression '&' expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_BAND, $3); }
            | expression '|' expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_BOR, $3); }   
            | expression "==" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_EQ, $3); }
            | expression "!=" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_NE, $3); }
            | expression ">" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_GT, $3); }
            | expression "<" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_LT, $3); }
            | expression "<=" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_LE, $3); }
            | expression ">=" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_GE, $3); }
            | expression "&&" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_AND, $3); }
            | expression "||" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_OR, $3); }
            | expression "<<" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_LSH, $3); }
            | expression ">>" expression
                { $$ = binexpr((M1_compiler *)yyget_extra(yyscanner), $1, OP_RSH, $3); }   
                                      
                                    
            ;
           
return_type : type    { $$ = $1; }
            | "void"  { $$ = "void"; }
            ;
            
type    : native_type   
              {
                 M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);
                 comp->parsingtype = $1;
                 $$ = $1;  
              } 
        | TK_USERTYPE
              {
                 M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);         
                 comp->parsingtype = $1; 
                 $$ = $1; 
              }
                       
        ;
        
native_type : "int"     { $$ = "int"; }
            | "num"     { $$ = "num"; }
            | "string"  { $$ = "string"; }
            | "bool"    { $$ = "bool"; }
            | "char"    { $$ = "char"; }
            ;
            

/* Embedded M0 instructions in M1. */

/* TODO: handle M0 instructions */                        
m0_block    : "M0" '{' m0_instructions '}'
                { 
                  M1_compiler *comp = (M1_compiler *)yyget_extra(yyscanner);
                  $$ = expression(comp, EXPR_M0BLOCK); 
                }
            ;            
            
m0_instructions : m0_instr
                | m0_instructions m0_instr
                    { 
                      $1->next = $2; 
                      $$ = $1;
                    }
                ;
                
m0_instr    : m0_op m0_arg ',' m0_arg ',' m0_arg
                { $$ = NULL; /* instr($1, $2, $4, $6); */}
            | m0_op m0_arg ',' m0_arg ',' 'x'
                { $$ = NULL; /* instr($1, $2, $4, 0); */}
            | m0_op m0_arg ',' 'x' ',' 'x'
                { $$ = NULL; /*instr($1, $2, 0, 0); */}
            | m0_op 'x' ',' 'x' ',' 'x'
                { $$ = NULL; /*instr($1, 0, 0, 0); */}
            ;                            
            
m0_arg      : M0_INT_CONST  { $$=0; }
            
            /* add other argument types for M0 instructions */
            ;
            
m0_op       : "noop"            { $$ = M0_NOOP; }      
            | "goto"            { $$ = M0_GOTO; }
            | "goto_if"         { $$ = M0_GOTO_IF; }
            | "goto_chunk"      { $$ = M0_GOTO_CHUNK; }
            | "add_i"           { $$ = M0_ADD_I; }
            | "add_n"           { $$ = M0_ADD_N; }
            | "sub_i"           { $$ = M0_SUB_I; }
            | "sub_n"           { $$ = M0_SUB_N; }
            | "mult_i"          { $$ = M0_MULT_I; }
            | "mult_n"          { $$ = M0_MULT_N; }
            | "div_i"           { $$ = M0_DIV_I; }
            | "div_n"           { $$ = M0_DIV_N; }
            | "mod_i"           { $$ = M0_MOD_I; }    
            | "mod_n"           { $$ = M0_MOD_N; }    
            | "iton"            { $$ = M0_ITON; }
            | "ntoi"            { $$ = M0_NTOI; }    
            | "ashr"            { $$ = M0_ASHR; }    
            | "lshr"            { $$ = M0_LSHR; }
            | "shl"             { $$ = M0_SHL; }
            | "and"             { $$ = M0_AND; }
            | "or"              { $$ = M0_OR; }
            | "xor"             { $$ = M0_XOR; }
            | "gc_alloc"        { $$ = M0_GC_ALLOC; }    
            | "sys_alloc"       { $$ = M0_SYS_ALLOC; }
            | "sys_free"        { $$ = M0_SYS_FREE; }
            | "copy_mem"        { $$ = M0_COPY_MEM; }
            | "set"             { $$ = M0_SET; }
            | "set_imm"         { $$ = M0_SET_IMM; }
            | "deref"           { $$ = M0_DEREF; }
            | "set_ref"         { $$ = M0_SET_REF; }
            | "set_byte"        { $$ = M0_SET_BYTE; }
            | "get_byte"        { $$ = M0_GET_BYTE; }
            | "set_word"        { $$ = M0_SET_WORD; }
            | "get_word"        { $$ = M0_GET_WORD; }
            | "csym"            { $$ = M0_CSYM; }
            | "ccall_arg"       { $$ = M0_CCALL_ARG; }    
            | "ccall_ret"       { $$ = M0_CCALL_RET; }    
            | "ccall"           { $$ = M0_CCALL; }
            | "print_s"         { $$ = M0_PRINT_S; }
            | "print_i"         { $$ = M0_PRINT_I; }
            | "print_n"         { $$ = M0_PRINT_N; }
            | "exit"            { $$ = M0_EXIT; }                    
            ;

/* END */
            
%%


       
