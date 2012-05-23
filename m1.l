%{
    
#include "m1parser.h"
#include <string.h>

#define YY_EXTRA_TYPE  struct M1_compiler *

%}


%option nounput
%option reentrant
%option yylineno
%option bison-bridge
%option warn
%option noyywrap
%option outfile="m1lexer.c"
%option header-file="m1lexer.h"

%x M0_PARSING

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

{WS}                    { /* nothing */ }

[#].*{EOL}{WS}*         { /* ignore line comments */ }

{EOL}[\t\r\n ]*         { /* */ }

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

[\[\]{}();=+-/*^,\.]    { return yytext[0]; }
[?%&!:]                 { return yytext[0]; }

"break"                 { return KW_BREAK; }
"case"                  { return KW_CASE; }
"const"                 { return KW_CONST; }
"default"               { return KW_DEFAULT; }
"do"                    { return KW_DO; }
"else"                  { return KW_ELSE; }
"extends"				{ return KW_EXTENDS; }
"for"                   { return KW_FOR; }
"if"                    { return KW_IF; }
"int"                   { return KW_INT; }

"M0"                    { BEGIN(M0_PARSING); return KW_M0; }

"method"				{ return KW_METHOD; }
"namespace"             { return KW_NAMESPACE; }
"new"					{ return KW_NEW; }
"null"                  { return KW_NULL; }
"num"                   { return KW_NUM; }

"pmc"					{ return KW_PMC; }
"print"                 { return KW_PRINT; }
"return"                { return KW_RETURN; }
"self"					{ return KW_SELF; }
"string"                { return KW_STRING; }
"struct"                { return KW_STRUCT; }
"super"					{ return KW_SUPER; }
"switch"                { return KW_SWITCH; }
"void"                  { return KW_VOID; }
"vtable"				{ return KW_VTABLE; }
"while"                 { return KW_WHILE; }

{DQ_STRING}             {
                          yylval->sval = strdup(yytext);
                          return TK_STRING_CONST;
                        }

{DIGITS}                { 
                           yylval->ival = atoi(yytext); 
                           return TK_INT; 
                        }
                        
{FLOATNUM}              {
                           yylval->fval = atof(yytext);
                           return TK_NUMBER;
                        }
                        
[a-zA-Z_][a-zA-Z0-9]*   { 
                           yylval->sval = strdup(yytext);
                           return TK_IDENT; 
                        }

<M0_PARSING>{WS}        { /* do nothing */ }
<M0_PARSING>{EOL}[\t\r\n ]*         {  }
<M0_PARSING>[{,]         { return yytext[0]; }
<M0_PARSING>[}]         { BEGIN INITIAL; return yytext[0]; }
<M0_PARSING>[0-9]+      { yylval->ival = atoi(yytext); return M0_NUMBER; }

%{
/*   
<M0_PARSING>"noop"        { return M0_NOOP; }
<M0_PARSING>"goto"        { return M0_GOTO; }
<M0_PARSING>"goto_if"     { return M0_GOTO_IF; }
<M0_PARSING>"goto_chunk"  { return M0_GOTO_CHUNK; }
<M0_PARSING>"add_i"       { return M0_ADD_I; }
<M0_PARSING>"add_n"       { return M0_ADD_N; }
<M0_PARSING>"sub_i"       { return M0_SUB_I; }
<M0_PARSING>"sub_n"       { return M0_SUB_N; }
<M0_PARSING>"mult_i"      { return M0_MULT_I; }
<M0_PARSING>"mult_n"      { return M0_MULT_N; }
<M0_PARSING>"div_i"       { return M0_DIV_I; }
<M0_PARSING>"div_n"       { return M0_DIV_N; }
<M0_PARSING>"mod_i"       { return M0_MOD_I; }
<M0_PARSING>"mod_n"       { return M0_MOD_N; }
<M0_PARSING>"iton"        { return M0_ITON; }
<M0_PARSING>"ntoi"        { return M0_NTOI; }
<M0_PARSING>"ashr"        { return M0_ASHR; }
<M0_PARSING>"lshr"        { return M0_LSHR; }
<M0_PARSING>"shl"         { return M0_SHL; }
<M0_PARSING>"and"         { return M0_AND; }
<M0_PARSING>"or"          { return M0_OR; }
<M0_PARSING>"xor"         { return M0_XOR; }
<M0_PARSING>"gc_alloc"    { return M0_GC_ALLOC; }
<M0_PARSING>"sys_alloc"   { return M0_SYS_ALLOC; }
<M0_PARSING>"sys_free"    { return M0_SYS_FREE; }
<M0_PARSING>"copy_mem"    { return M0_COPY_MEM; }
<M0_PARSING>"set"         { return M0_SET; }
*/
%}
