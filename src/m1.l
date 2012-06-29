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
%option noinput

/* override memory allocation function with Parrot's memory functions;
 * don't generate built-ins.
 */
%option noyyalloc
%option noyyrealloc
%option noyyfree

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
">>"                    { return TK_RSH; /* arithmetic right shift. */}
">>>"                   { return TK_LRSH; /* logical right shift. */}

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
"^="                    { return TK_XOR_ASSIGN; }

[\[\]{}();=+-/*^,\.]    { return yytext[0]; }
[%&:|~]                 { return yytext[0]; }

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
"union"                 { return KW_UNION; }
"unsigned"              { return KW_UNSIGNED; }
"void"                  { return KW_VOID; }
"vtable"				{ return KW_VTABLE; }
"while"                 { return KW_WHILE; }

{DQ_STRING}             {
                          yylval->sval = strdup(yytext);
                          return TK_STRING_CONST;
                        }

\'.\'                   { /* match chars. */
                           yylval->cval = yytext[1];
                           return TK_CHAR;
                        }
          
{HEX}                   { yylval->ival = strtoul(yytext + 2, NULL, 16); return TK_INT; }
{OCT}                   { yylval->ival = strtoul(yytext + 2, NULL, 8); return TK_INT; }
{BIN}                   {  /* skip the "0b" prefix. */
                           yylval->ival = strtoul(yytext + 2, NULL, 2);
                           return TK_INT;
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
                               m1_type *decl = type_find_def(comp, yytext);
                       
                               if (decl == NULL) { /* it's not a user-defined type name. */
                               
                                   /* maybe it's an enum const.... */
                                   m1_enumconst *econst = type_find_enumconst(comp, yytext);
                                   if (econst == NULL) 
                                       return TK_IDENT; /* no, it wasn't so must be an ident. */
                                   else {
                                       /* it was an enum const, so return its value. */
                                       yylval->ival = econst->value;
                                       return TK_INT;
                                   }
                               }                            
                               else {
                                  return TK_USERTYPE;
                               }
                           }
                           else {
                               m1_enumconst *econst = type_find_enumconst(comp, yytext);
                               if (econst == NULL) 
                                   return TK_IDENT;
                               else {
                                   yylval->ival = econst->value;
                                   return TK_INT;
                               }
                                
                           }
                           
                        }

.                       { yyerror(yyscanner, yyget_extra(yyscanner), yytext); }



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


