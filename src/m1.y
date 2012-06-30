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
#include "symtab.h"


extern int yylex(YYSTYPE *yylval, yyscan_t yyscanner);


/*

Parse errors are handled through yyerror.
Increase the global (stored in comp) error count.

*/
int 
yyerror(yyscan_t yyscanner, M1_compiler *comp, const char *str) {

    fprintf(stderr, "%s: unexpected token '%s' (line %d)\n", 
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
    struct m1_enum          *enm;
    struct m1_structfield   *sfld;
    struct m1_var           *var;
    struct m0_instr         *instr;
    struct m1_case			*cse;
    struct m1_enumconst     *ecnst;
    struct m1_block         *blck;
    struct m1_dimension     *dim;
    struct m1_symbol        *sym;
    struct m1_ident         *ident;
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
        TK_LRSH         ">>>"
        TK_STRING_CONST
        TK_INC_ASSIGN   "+="
        TK_DEC_ASSIGN   "-="
        TK_MUL_ASSIGN   "*="
        TK_DIV_ASSIGN   "/="
        TK_XOR_ASSIGN   "^="
        TK_MOD_ASSIGN   "%="
        TK_SHR_ASSIGN   ">>="
        TK_SHL_ASSIGN   "<<="
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
        KW_UNION        "union"

        
%type <sval> TK_IDENT
             TK_STRING_CONST
             return_type 
             type 
             __type
             native_type 
             TK_USERTYPE
             
%type <chunk> function_definition 
              function_init
              chunks 
              chunk 
              TOP
              method_init
              pmc_method
              pmc_methods


             
%type <ival> TK_INT 
             opt_vtable
             struct_or_union
             
%type <dim> dimension                   
             
%type <ival> assignop
             opt_enum_val

%type <ecnst> enum_const
              enum_constants
             
%type <cval> TK_CHAR

%type <fval> TK_NUMBER

%type <expr> expression              
             binexpr 
             inc_or_dec_expr 
             function_call_expr 
             function_call_stat 
             lvalue
             rvalue
             statement 
             statements 
             block
             inc_or_dec_stat 
             opt_init
             assign_stat
             assign_expr
             chained_assign_expr
             if_stat
             while_stat
             do_stat
             for_stat             
             arguments
             for_init
             for_inits
             init
             for_cond
             for_step
             for_steps
             step
             return_stat
             break_stat
             continue_stat
             tertexpr
             constexpr
             switch_stat
             const_declaration
             var_declaration
             unexpr
             print_stat
             default_case
             arrayconstructor
             nullexpr
             subexpr
             newexpr
             opt_array_init
             opt_ret_expr
             expr_list
             const_list


%type <blck>  open_block
             
%type <var>  var
             var_list
             param
             param_list
             parameters              
              
%type <var>  struct_members 
             struct_member

             
%type <strct> struct_definition
              struct_init
              pmc_definition    
              pmc_init

%type <enm> enum_definition
             
%type <obj> field_access             
            lhs_obj
        
%type <cse> case 
            cases

%type <ident> id_list
              extends_clause
        
%token TK_USERTYPE

        
%pure-parser

%parse-param	{yyscan_t yyscanner}
%lex-param		{yyscan_t yyscanner}
%parse-param	{struct M1_compiler * const comp}

%defines
%expect 0
%error-verbose

%start TOP


%left  ':' 
%right TK_INC_ASSIGN '='
%left TK_AND TK_OR 
%left TK_LE TK_GE TK_LT TK_GT TK_EQ TK_NE
%left TK_LSH TK_RSH TK_LRSH
%left '+' '-' 
%left '*' '/' '&' '|' '%'  '^' '~'
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
            { comp->ast = $2; }
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
           { $$ = NULL; }
        | enum_definition
           { $$ = NULL; }
        ;        


enum_definition : "enum" TK_IDENT '{' enum_constants '}'
                    {                       
                      $$ = newenum(comp, $2, $4 ); 
                      type_enter_enum(comp, $2, $$);
                    }
                ;
        
enum_constants  : enum_const 
                    { $$ = $1; }
                | enum_constants ',' enum_const 
                    { 
                      /* link in reverse order. */
                      $3->next = $1;
                      $$ = $3;    
                    }
                ; 
                  
enum_const      : TK_IDENT opt_enum_val
                    { $$ = enumconst(comp, $1, $2); }
                ;
                
opt_enum_val    : /* empty */
                    { /* if no value is specified, get one from comp. */
                       $$ = comp->enum_const_counter++; 
                    }
                | '=' TK_INT
                    { 
                       /* a specified value for an enum const; the NEXT enum
                          without a specified value will have THIS value + 1.
                          Therefore, update comp->enum_const_counter.                          
                        */                      
                       comp->enum_const_counter = $2 + 1;
                       $$ = $2;                        
                    }
                ;
                   
namespace_definition: "namespace" TK_IDENT ';'
                         { comp->current_namespace = $2; }    
                    ;    

struct_definition   : struct_init '{' struct_members '}' 
                        { 
                          comp->currentsymtab = NULL; /* otherwise it might be linked as a 
                                                         parent symtab for a chunk. */

                        }
                    ;       
                    
struct_init         : struct_or_union TK_IDENT
                        {
                          $$ = newstruct(comp, $2, NULL); /* make AST node for this definition. */
                          type_enter_struct(comp, $2, $$); /* enter into type definitions. */
                          comp->currentsymtab = &$$->sfields; /* make symbol table easily accessible. */
                          $$->is_union = $1; 
                        }
                    ;  
                    
struct_or_union     : "struct"  { $$ = 0; }
                    | "union"   { $$ = 1; }
                    ;                                        
                    
struct_members      : struct_member   
                    | struct_members struct_member                                                  
                    ;

/* struct_members are handled in a similar way as vars, but the grammar rules are slightly. */

struct_member       : type TK_IDENT ';'
                        {  
                          comp->parsingtype = $1;                            
                          $$ = var(comp, $2, NULL); 
                        }
                    | type TK_IDENT dimension ';'
                        { 
                          comp->parsingtype = $1;                            
                          $$ = array(comp, $2, $3, NULL); 
                        }
                    ; 

pmc_definition	: pmc_init '{'  struct_members pmc_methods '}'
                    {
                      comp->currentsymtab = NULL; /* otherwise it might be linked as a 
                                                     parent symtab for a chunk. */
                    }
                ;
                
pmc_init        : "pmc" TK_IDENT extends_clause
                    {          
                       $$ = newstruct(comp, $2, $3); 
                       type_enter_pmc(comp, $2, $$);
                       /* point to this PMC's symbol table. */
                       comp->currentsymtab = &$$->sfields;
                    }
                ;                
                                
extends_clause	: /* empty */
                    { $$ = NULL; }
                | "extends" id_list
                    { $$ = $2; }
                ;
                
id_list			: TK_IDENT
                    { $$ = identlist(NULL, $1); }
                | id_list ',' TK_IDENT
                    { $$ = identlist($1, $3); }
                ;                

                
                
pmc_methods     : /* empty */
                    { $$ = NULL; }
                | pmc_methods pmc_method
                    {
                       /* link in reverse order. */
                       $2->next = $1;
                       $$ = $2;    
                    }
                ;                                


pmc_method		: method_init '(' parameters ')' open_block statements '}' 
                    {                         
                       $1->block = $5;
                       block_set_stat($5, $6);
                       $$ = $1;    
                       
                       add_chunk_parameters(comp, $$, $3, CHUNK_ISMETHOD);
                                              
                       /* now close the scope. */
                       close_scope(comp);                      
                    }
				;
				
method_init     : opt_vtable "method" type TK_IDENT	
                    {
                      $$ = chunk(comp, $3, $4, $1 | CHUNK_ISMETHOD);
                      comp->currentchunk = $$;                         
                    }			
                ;
				
opt_vtable      : /* empty */   { $$ = 0; }
                | "vtable"      { $$ = CHUNK_ISVTABLE; }
                ;				
                                        
function_definition : function_init '(' parameters ')' open_block statements '}' 
                        {  
                          /* we only want the m1_block object, so remove its m1_expression wrapper. */
                          $1->block = $5; 
                          /* store the list of statements ($6) in the block ($5). */
                          block_set_stat($5, $6);    
                          
                          $$ = $1;

                          add_chunk_parameters(comp, $$, $3, CHUNK_ISFUNCTION);
                                                                                                                                  
                          /* now close the scope. */
                          close_scope(comp);                      

                        }
                    ;

function_init   : return_type TK_IDENT
                        {
                          /* create a new chunk so we can set it as "current" before
                             parsing the remainder of the function. Parameters and
                             statements (which include var. decl.) can then use this
                             "current" chunk (for its symbol table etc.). 
                           */  
                          $$ = chunk(comp, $1, $2, 0); 
                          comp->currentchunk = $$;

                          
                          /* enter name of this function in global symbol table, so
                             compiler can find it whenever another function calls this one.
                           */
                          $$->sym = sym_new_symbol(comp, comp->globalsymtab, $2, $1, 1);
                          $$->sym->chunk = $$;
                        }
                ;

parameters  : /* empty */
                { $$ = NULL; }
            | param_list
                { $$ = $1; }
            ;
            
param_list  : param
                { $$ = $1; }
            | param_list ',' param
                {  
                    /* store them in reverse order. (for now. XXX) */
                    $3->next = $1;
                    $$ = $3;    
                }
            ;            
        
param   : type TK_IDENT         { $$ = parameter(comp, $1, $2); }
        | type '*' TK_IDENT     { $$ = parameter(comp, $1, $3); }
        ;                                                                                   
        
block   : open_block statements close_block
            {  
                /* a <block> isa <statement>, so need to wrap it as a m1_expression. */
                m1_expression *e = expression(comp, EXPR_BLOCK);
                e->expr.as_block     = $1; /* store block in the expr union of e. */
                block_set_stat($1, $2);
                $$ = e;
            }
        ;
        
open_block: '{'   /* create a new block, set currentsymtab to its symbol table. */
                  { $$ = open_scope(comp); }

close_block: '}'  /* close the current block, restore currentsymtab to the enclosing scope's symtab. */
                  { close_scope(comp); }
                          
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
                                    
print_stat  : "print" '(' arguments ')' ';'
                { $$ = printexpr(comp, $3); }
            ;

                  
const_declaration   : "const" type TK_IDENT '=' constexpr ';'
                        { $$ = constdecl(comp, $2, $3, $5); }
                    ;                  
                        
var_declaration: type var_list ';'  
                    { $$ = vardecl(comp, $1, $2); }            
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
                { $$ = var(comp, $1, $2); }
            | TK_IDENT dimension opt_array_init
                { $$ = array(comp, $1, $2, $3); }
            ;           
            
dimension   : '[' TK_INT ']'
                { $$ = array_dimension($2); }
            | dimension '[' TK_INT ']'
                { 
                  m1_dimension *iter = $1;
                  while (iter->next != NULL)
                    iter = iter->next;
                  /* out of while loop; iter->next is now NULL */   
                  iter->next = array_dimension($3);
                  $$ = $1;  
                }
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
            
assign_expr : lvalue assignop rvalue
                { $$ = assignexpr(comp, $1, $2, $3); }            
            | chained_assign_expr
            ;
            
chained_assign_expr: lvalue '=' rvalue
                        { $$ = assignexpr(comp, $1, OP_ASSIGN, $3); }
                   | lvalue '=' chained_assign_expr
                        {                         
                          /* The parse tree for:
                          
                          a = b = c = 42;
                          
                          should look like this:
                          
                                   =
                                 /   \
                                a     =
                                     /  \
                                    b    =
                                        /  \
                                       c    42
                               
                          This rule is *right* recursive, which is not very
                          efficient in LALR parsers such as generated by Bison.
                          However, nesting is unlikely to go very deep.
                          */
                          $$ = assignexpr(comp, $1, OP_ASSIGN, $3);
                          
                        }
                   ;
            
assignop    : "+="  { $$ = OP_PLUS; }
            | "-="  { $$ = OP_MINUS; }
            | "*="  { $$ = OP_MUL; }
            | "/="  { $$ = OP_DIV; }
            | "%="  { $$ = OP_MOD; }
            | ">>=" { $$ = OP_RSH; }
            | "<<=" { $$ = OP_LSH; }
            | "|="  { $$ = OP_BOR; }
            | "&="  { $$ = OP_BAND; }
            | "^="  { $$ = OP_XOR; }
            ;            
            
           
if_stat     : "if" '(' expression ')' statement %prec LOWER_THAN_ELSE  
                { $$ = ifexpr(comp, $3, $5, NULL); }
            | "if" '(' expression ')' statement "else" statement 
                { $$ = ifexpr(comp, $3, $5, $7); }
            ;
            
            
while_stat  : "while" '(' expression ')' statement 
                { $$ = whileexpr(comp, $3, $5); }
            ;
            
do_stat     : "do" block "while" '(' expression ')' ';' 
                { $$ = dowhileexpr(comp, $5, $2); }
            ;
            
switch_stat : "switch" '(' expression ')' '{' cases default_case '}'
                { $$ = switchexpr(comp, $3, $6, $7); }
            ;
            
cases       : /* empty */
				{ $$ = NULL; }
            | cases case 
            	{   /* link them in reverse order as order doesn't matter. */
            	    
            	    /* Note that $1 may be NULL (first time matching this rule). */
            		$2->next = $1;
            		$$ = $2; 
            	}
            ;
            
case        : "case" TK_INT ':' statements                       
				{ $$ = switchcase(comp, $2, $4); }
            ;
            
default_case: /* empty */
				{ $$ = NULL; }
            | "default" ':' statements
            	{ $$ = $3; }
            ;
                       
function_call_expr  : lvalue '(' arguments ')' 
                         { $$ = funcall(comp, $1->expr.as_object, $3); }
                    ;
                    
function_call_stat  : function_call_expr ';'
                         { $$ = $1; }
                    ;

arguments   : /* empty */
                { $$ = NULL; }
            | expr_list
                { $$ = $1; }
            ;
            
expr_list   : expression 
                { $$ = $1; }
            | expr_list ',' expression 
                { 
                  /* link them in reverse order for now. */
                  $3->next = $1;
                  $$ = $3;   
                }
            ;                                                    
            
for_stat    : "for" '(' for_init ';' for_cond ';' for_step ')' statement
                {                   
                  /* If for_init contains an iterator declaration, enter its declaration in
                     a block's symbol table. 
                  */
                  if ($3 != NULL) {
                      if ($3->type == EXPR_VARDECL) {
                        
                          if ($9->type != EXPR_BLOCK) {
                            /* a single statement, make it a block. */
                            m1_block *b = block(comp);
                            b->locals.parentscope = comp->currentsymtab;
                            block_set_stat(b, $9);
                            $9 = expression(comp, EXPR_BLOCK);
                            $9->expr.as_block = b;
                            
                          }
                          $3->expr.as_var->sym = sym_new_symbol(comp, 
                                      &($9->expr.as_block->locals), 
                                      $3->expr.as_var->name, 
                                      $3->expr.as_var->type, 
                                      1);  
                        }
                    }
                
                  $$ = forexpr(comp, $3, $5, $7, $9);                 
                }
            ;  
            
            
for_init    : /* empty */
                { $$ = NULL; }
            | for_inits
            ;
            
for_inits   : init
            | for_inits ',' init
                {
                 $3->next = $1;
                 $$ = $3;    
                }
            ;
            
init        : assign_expr
            | type TK_IDENT '=' expression 
                { 
                    m1_var *v;
                    comp->parsingtype = $1;
                    v = make_var(comp, $2, $4, 1);
                    $$ = vardecl(comp, $1, v);                 
                
                }
            ;
            
for_cond    : /* empty */
                { $$ = NULL; }
            | expression
            ;
            
for_step    : /* empty */
                { $$ = NULL; }
            | for_steps
            ;
            
for_steps   : step
            | for_steps ',' step
                {
                   $3->next = $1;
                   $$ = $3;   
                }
                
            ;
            
step        : expression
            | assign_expr
            ;                                                                      
            

inc_or_dec_expr : lvalue "++"
                    { $$ = inc_or_dec(comp, $1, UNOP_POSTINC); }
                | lvalue "--"
                    { $$ = inc_or_dec(comp, $1, UNOP_POSTDEC); }
                | "++" lvalue
                    { $$ = inc_or_dec(comp, $2, UNOP_PREINC); }
                | "--" lvalue
                    { $$ = inc_or_dec(comp, $2, UNOP_PREDEC); }                    
                ;
                
inc_or_dec_stat : inc_or_dec_expr ';'
                    { $$ = $1; }
                ;

break_stat      : "break" ';'
                    { $$ = expression(comp, EXPR_BREAK); }                
                ;
                
continue_stat   : "continue" ';'
                    { $$ = expression(comp, EXPR_CONTINUE); }                
                ;
                
return_stat     : "return" opt_ret_expr ';'
                    { $$ = returnexpr(comp, $2); }
                ;   
            
opt_ret_expr    : /* empty */     { $$ = NULL; }
                | expression      { $$ = $1; }
                ;                         
                            
lvalue  : lhs_obj
           { $$ = objectexpr(comp, $1, EXPR_OBJECT); }           
        | '*' lhs_obj
           { $$ = objectexpr(comp, $2, EXPR_DEREF); }
        | '&' lhs_obj
           { $$ = objectexpr(comp, $2, EXPR_ADDRESS); }
        ;
        
lhs_obj : TK_IDENT
            { 
              $$ = object(comp, OBJECT_MAIN); 
              obj_set_ident($$, $1);
            }            
        | lhs_obj field_access
            {
              /* make a new node that links $1 and $2. */
              $$ = lhsobj(comp, $1, $2);                                          
            }
        | "self"
        	{ $$ = object(comp, OBJECT_SELF); }
        | "super"
        	{ $$ = object(comp, OBJECT_SUPER); }
        ;        
        
field_access: '[' expression ']'
                { $$ = arrayindex(comp, $2); }                
            | '.' TK_IDENT
                { $$ = objectfield(comp, $2); }           
            | "->" TK_IDENT
                { $$ = objectderef(comp, $2); }                
            | "::" TK_IDENT
                { $$ = NULL; /* do we want this scope operator? */}
            ;        

rvalue  : expression
        ;
        
constexpr   : TK_NUMBER    
                { $$ = number(comp, $1); }    
            | TK_INT
                { $$ = integer(comp, $1); }  
            | TK_STRING_CONST
                { $$ = string(comp, $1); }
            | "true"
                { $$ = expression(comp, EXPR_TRUE); }
            | "false"
                { $$ = expression(comp, EXPR_FALSE); }
            | TK_CHAR
                { $$ = character(comp, $1); } 
            ;
            
expression  : constexpr 
            | inc_or_dec_expr                
            | subexpr
            | unexpr             
            | binexpr
            | tertexpr
            | lvalue                
            | function_call_expr                
            | nullexpr           
            | newexpr
            ;
            
subexpr     : '(' expression ')'
                { $$ = $2; }           
            ;
            
            
newexpr     : "new" TK_USERTYPE '(' arguments ')'
                { $$ = newexpr(comp, $2, $4); }
            ;         
            
nullexpr    : "null"
                { $$ = expression(comp, EXPR_NULL); }            
            ;
            
arrayconstructor: '{' const_list '}' 
                     { $$ = $2; }
                ;  
            
                           
const_list    : constexpr                    
              | const_list ',' constexpr            
                { 
                  m1_expression *iter = $1;
                  /* need to link in correct order; find end of list. */
                  while (iter->next != NULL) 
                    iter = iter->next;
                  
                  iter->next = $3;
                  $$ = $1;                                           
                }
              ;
            
unexpr  : '-' expression
               { $$ = binexpr(comp, $2, OP_MUL, integer(comp, -1)); }                                          
        | '(' return_type ')' expression %prec LOWER_THAN_ELSE
                { $$ = castexpr(comp, $2, $4); }
        | "!" expression 
                { $$ = unaryexpr(comp, UNOP_NOT, $2); }                        
        | '~' expression 
        
        /* bitwise NOT:  ~x == -x -1. See http://en.wikipedia.org/wiki/Bitwise_operation#NOT. 
         * -x is implemented as x * -1. Implement this in the parser, as we need access to 
         * the constants segment. 
         */        
                { /*$$ = unaryexpr(comp, UNOP_BNOT, $2);*/                                  
                   /* create a node for "-x" => "x * -1" */
                   m1_expression *minusX = binexpr(comp, $2, OP_MUL, integer(comp, -1));
                   /* create a node for (-x) - (1). Note it's not - (-1), as that results in +1.*/
                   $$ = binexpr(comp, minusX, OP_MINUS, integer(comp, 1));                                      
                }
        ;            
       
tertexpr    : expression "?" expression ':' expression
                { $$ = ifexpr(comp, $1, $3, $5); }
            ;
                   
binexpr     : expression '+' expression
                { $$ = binexpr(comp, $1, OP_PLUS, $3); }
            | expression '-' expression
                { $$ = binexpr(comp, $1, OP_MINUS, $3); }
            | expression '*' expression
                { $$ = binexpr(comp, $1, OP_MUL, $3); }
            | expression '/' expression
                { $$ = binexpr(comp, $1, OP_DIV, $3); }
            | expression '%' expression
                { $$ = binexpr(comp, $1, OP_MOD, $3); }
            | expression '^' expression
                { $$ = binexpr(comp, $1, OP_XOR, $3); }
            | expression '&' expression
                { $$ = binexpr(comp, $1, OP_BAND, $3); }
            | expression '|' expression
                { $$ = binexpr(comp, $1, OP_BOR, $3); }   
            | expression "==" expression
                { $$ = binexpr(comp, $1, OP_EQ, $3); }
            | expression "!=" expression
                { $$ = binexpr(comp, $1, OP_NE, $3); }
            | expression ">" expression
                { $$ = binexpr(comp, $1, OP_GT, $3); }
            | expression "<" expression
                { $$ = binexpr(comp, $1, OP_LT, $3); }
            | expression "<=" expression
                { $$ = binexpr(comp, $1, OP_LE, $3); }
            | expression ">=" expression
                { $$ = binexpr(comp, $1, OP_GE, $3); }
            | expression "&&" expression
                { $$ = binexpr(comp, $1, OP_AND, $3); }
            | expression "||" expression
                { $$ = binexpr(comp, $1, OP_OR, $3); }
            | expression "<<" expression
                { $$ = binexpr(comp, $1, OP_LSH, $3); }
            | expression ">>" expression
                { $$ = binexpr(comp, $1, OP_RSH, $3); }                                                                             
            | expression ">>>" expression
                { $$ = binexpr(comp, $1, OP_LRSH, $3); }                                                                                             
            ;
           
return_type : type    { $$ = $1; }
            | "void"  { $$ = "void"; }
            ;
            
type        : __type   
               { $$ = comp->parsingtype = $1; }         
            ;

/* __type is a helper rule to prevent code duplication. */              
__type      : native_type
            | TK_USERTYPE                                          
            ;
        
native_type : "int"     { $$ = "int"; }
            | "num"     { $$ = "num"; }
            | "string"  { $$ = "string"; }
            | "bool"    { $$ = "bool"; }
            | "char"    { $$ = "char"; }
            ;
            

            
%%


       
