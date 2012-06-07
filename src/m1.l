%{
    
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include "m1parser.h"
#include "compiler.h"
#include "decl.h"

#define YY_EXTRA_TYPE  struct M1_compiler *

extern int yyerror(yyscan_t yyscanner, M1_compiler *comp, char *str);




%}


%option nounput
%option reentrant
%option yylineno
%option bison-bridge
%option warn
%option noyywrap
%option stack
%option never-interactive
%option pointer
%option noyy_top_state

/* override memory allocation function with Parrot's memory functions;
 * don't generate built-ins.
 */
%option noyyalloc
%option noyyrealloc
%option noyyfree

%x M0_PARSING
%x MULTLINECOMMENT

WS              [\t\f\r\x1a ]
EOL             \r?\n
HEX             0[xX][0-9A-Fa-f]+
OCT             0[oO][0-7]+
BIN             0[bB][01]+

DOT             [.]
DIGIT           [0-9]
DIGITS          {DIGIT}+

DQ_STRING       \"(\\.|[^"\\\n])*\"
SQ_STRING       \'[^'\n]*\'
Q_STRING        {SQ_STRING}|{DQ_STRING}

SIGN            [-+]
BIGINT          {SIGN}?{DIGITS}"L"
FLOATNUM        {SIGN}?(({DIGITS}{DOT}{DIGIT}*|{DOT}{DIGITS})([eE]{SIGN}?{DIGITS})?|{DIGITS}[eE]{SIGN}?{DIGITS})
%%

{WS}                    { /* ignore white space. */ }

"//".*{EOL}{WS}*        { /* ignore line comments. */ }

"/*"                    { yy_push_state(MULTLINECOMMENT, yyscanner); }
<MULTLINECOMMENT>"/*"   { yy_push_state(MULTLINECOMMENT, yyscanner); }
<MULTLINECOMMENT>"*/"   { yy_pop_state(yyscanner); }
<MULTLINECOMMENT>.      { /* ignore anything in multi line comments. */ }
<MULTLINECOMMENT>{EOL}  { /* ignore newlines in multi line comments. */ }

{EOL}[\t\r\n ]*         { /* skip these characters */ }

"->"                    { return TK_ARROW; }
"||"                    { return TK_OR; }
"&&"                    { return TK_AND; }
"++"                    { return TK_INC; }
"--"                    { return TK_DEC; }
"=="                    { return TK_EQ; }
"!="                    { return TK_NE; }
">="                    { return TK_GE; }
"<="                    { return TK_LE; }
"<"                     { return TK_LT; }
">"                     { return TK_GT; }

"<<"                    { return TK_LSH; }
">>"                    { return TK_RSH; }

"::"                    { return TK_SCOPE; }

"+="                    { return TK_INC_ASSIGN; }
"-="                    { return TK_DEC_ASSIGN; }
"*="                    { return TK_MUL_ASSIGN; }
"/="                    { return TK_DIV_ASSIGN; }
"%="                    { return TK_MOD_ASSIGN; }
"<<="                   { return TK_SHL_ASSIGN; }
">>="                   { return TK_SHR_ASSIGN; }
"|="                    { return TK_BOR_ASSIGN; }
"&="                    { return TK_BAND_ASSIGN; }

[\[\]{}();=+-/*^,\.]    { return yytext[0]; }
[%&:]                   { return yytext[0]; }

"?"                     { return TK_ISTRUE; }
"!"                     { return TK_NOT; }
"bool"                  { return KW_BOOL; }
"break"                 { return KW_BREAK; }
"case"                  { return KW_CASE; }
"catch"                 { return KW_CATCH; }
"char"                  { return KW_CHAR; }
"const"                 { return KW_CONST; }
"continue"              { return KW_CONTINUE; }
"default"               { return KW_DEFAULT; }
"do"                    { return KW_DO; }
"else"                  { return KW_ELSE; }
"enum"                  { return KW_ENUM; }
"extends"				{ return KW_EXTENDS; }
"extern"                { return KW_EXTERN; }
"false"                 { return KW_FALSE; }
"for"                   { return KW_FOR; }
"if"                    { return KW_IF; }
"import"                { return KW_IMPORT; }
"inline"                { return KW_INLINE; }
"int"                   { return KW_INT; }

"M0"                    { BEGIN(M0_PARSING); return KW_M0; }

"method"				{ return KW_METHOD; }
"namespace"             { return KW_NAMESPACE; }
"new"					{ return KW_NEW; }
"null"                  { return KW_NULL; }
"num"                   { return KW_NUM; }

"pmc"					{ return KW_PMC; }
"print"                 { return KW_PRINT; }
"private"               { return KW_PRIVATE; }
"public"                { return KW_PUBLIC; }
"return"                { return KW_RETURN; }
"self"					{ return KW_SELF; }
"string"                { return KW_STRING; }

"struct"                { return KW_STRUCT; }

"super"					{ return KW_SUPER; }
"switch"                { return KW_SWITCH; }
"throw"                 { return KW_THROW; }
"true"                  { return KW_TRUE; }
"try"                   { return KW_TRY; }
"unsigned"              { return KW_UNSIGNED; }
"void"                  { return KW_VOID; }
"vtable"				{ return KW_VTABLE; }
"while"                 { return KW_WHILE; }

{DQ_STRING}             {
                          yylval->sval = strdup(yytext);
                          return TK_STRING_CONST;
                        }

\'.\'                   { /* match chars. */
                           yylval->cval = yytext[0];
                           return TK_CHAR;
                        }
                        
[1-9]{DIGIT}*|"0"       { 
                           yylval->ival = atoi(yytext); 
                           return TK_INT; 
                        }
                        
{FLOATNUM}              {
                           yylval->fval = atof(yytext);
                           return TK_NUMBER;
                        }
                        
[a-zA-Z_][a-zA-Z0-9_]*  { 
                           M1_compiler *comp = yyget_extra(yyscanner);
                           yylval->sval      = strdup(yytext);                   
                           
                           /* if parser is currently expecting a usertype ID
                              check to see if yytext is indeed the name of 
                              a user-defined type. Otherwise just return TK_IDENT.
                            */
                           if (comp->is_parsing_usertype == 1) {
                               m1_decl     *decl = type_find_def(comp, yytext);
                       
                               if (decl == NULL) { /* not found, so must be an identifier */
                                  fprintf(stderr, "%s is a TK_IDENT\n", yytext);
                                  return TK_IDENT; 
                               }                            
                               else {
                                  fprintf(stderr, "%s is a TK_USERTYPE\n", yytext);
                                 // comp->is_parsing_usertype = 0;
                                  return TK_USERTYPE;
                               }
                           }
                           else {
                                fprintf(stderr, "%s is a TK_IDENT\n", yytext);
                               return TK_IDENT;
                           }
                           
                        }

.                       { yyerror(yyscanner, yyget_extra(yyscanner), yytext); }

<M0_PARSING>{WS}             { /* do nothing */ }
<M0_PARSING>{EOL}[\t\r\n ]*  { /* ignore */ }
<M0_PARSING>[{,]             { return yytext[0]; }
<M0_PARSING>[}]              { BEGIN INITIAL; return yytext[0]; }
<M0_PARSING>[0-9]+           { yylval->ival = atoi(yytext); return TK_INT; }


<M0_PARSING>"noop"        { return KW_NOOP; }
<M0_PARSING>"goto"        { return KW_GOTO; }
<M0_PARSING>"goto_if"     { return KW_GOTO_IF; }
<M0_PARSING>"goto_chunk"  { return KW_GOTO_CHUNK; }
<M0_PARSING>"add_i"       { return KW_ADD_I; }
<M0_PARSING>"add_n"       { return KW_ADD_N; }
<M0_PARSING>"sub_i"       { return KW_SUB_I; }
<M0_PARSING>"sub_n"       { return KW_SUB_N; }
<M0_PARSING>"mult_i"      { return KW_MULT_I; }
<M0_PARSING>"mult_n"      { return KW_MULT_N; }
<M0_PARSING>"div_i"       { return KW_DIV_I; }
<M0_PARSING>"div_n"       { return KW_DIV_N; }
<M0_PARSING>"mod_i"       { return KW_MOD_I; }
<M0_PARSING>"mod_n"       { return KW_MOD_N; }
<M0_PARSING>"iton"        { return KW_ITON; }
<M0_PARSING>"ntoi"        { return KW_NTOI; }
<M0_PARSING>"ashr"        { return KW_ASHR; }
<M0_PARSING>"lshr"        { return KW_LSHR; }
<M0_PARSING>"shl"         { return KW_SHL; }
<M0_PARSING>"and"         { return KW_AND; }
<M0_PARSING>"or"          { return KW_OR; }
<M0_PARSING>"xor"         { return KW_XOR; }
<M0_PARSING>"gc_alloc"    { return KW_GC_ALLOC; }
<M0_PARSING>"sys_alloc"   { return KW_SYS_ALLOC; }
<M0_PARSING>"sys_free"    { return KW_SYS_FREE; }
<M0_PARSING>"copy_mem"    { return KW_COPY_MEM; }
<M0_PARSING>"set"         { return KW_SET; }
<M0_PARSING>"set_imm"     { return KW_SET_IMM; } 
<M0_PARSING>"deref"       { return KW_DEREF; }
<M0_PARSING>"set_ref"     { return KW_SET_REF; }  
<M0_PARSING>"set_byte"    { return KW_SET_BYTE; }
<M0_PARSING>"get_byte"    { return KW_GET_BYTE; }
<M0_PARSING>"set_word"    { return KW_SET_WORD; }
<M0_PARSING>"get_word"    { return KW_GET_WORD; }
<M0_PARSING>"csym"        { return KW_CSYM; }
<M0_PARSING>"ccall_arg"   { return KW_CCALL_ARG; }
<M0_PARSING>"ccall_ret"   { return KW_CCALL_RET; }
<M0_PARSING>"ccall"       { return KW_CCALL; }
<M0_PARSING>"print_s"     { return KW_PRINT_S; }
<M0_PARSING>"print_i"     { return KW_PRINT_I; }
<M0_PARSING>"print_n"     { return KW_PRINT_N; }
<M0_PARSING>"exit"        { return KW_EXIT; }


%%

/* declare these functions in order to be able to prevent warnings
   on unused yyscanner argument. Assert that they are not-NULL or
   NULL to prevent this warning, so they're used.
   
   XXX Later on, when we're doing our own memory allocation, yyscanner
   gives access to M1_compiler, which will keep track of memory.
 */

void *
yyalloc(yy_size_t size, yyscan_t yyscanner) {
    assert(yyscanner != NULL || yyscanner == NULL);
    return malloc(size);
}

void *
yyrealloc(void *mem, yy_size_t newsize, yyscan_t yyscanner) {
    assert(yyscanner != NULL || yyscanner == NULL);
    return realloc(mem, newsize);
}

void 
yyfree(void * mem, yyscan_t yyscanner) {
    assert(yyscanner != NULL || yyscanner == NULL);
    free(mem);
    mem = NULL;   
}


