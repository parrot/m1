
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TK_IDENT = 258,
     TK_NUMBER = 259,
     KW_NUM = 260,
     KW_INT = 261,
     TK_INT = 262,
     KW_STRUCT = 263,
     TK_INC = 264,
     TK_DEC = 265,
     KW_IF = 266,
     KW_WHILE = 267,
     KW_FOR = 268,
     KW_ELSE = 269,
     KW_DO = 270,
     TK_AND = 271,
     TK_OR = 272,
     TK_ARROW = 273,
     KW_VOID = 274,
     KW_NULL = 275,
     KW_BREAK = 276,
     KW_RETURN = 277,
     KW_CONST = 278,
     TK_GE = 279,
     TK_GT = 280,
     TK_LT = 281,
     TK_LE = 282,
     TK_EQ = 283,
     TK_NE = 284,
     KW_NAMESPACE = 285,
     TK_NS_SEP = 286,
     TK_LSH = 287,
     TK_RSH = 288,
     KW_M0 = 289,
     M0_NL = 290,
     M0_IDENT = 291,
     M0_NUMBER = 292,
     M0_ADD_I = 293,
     M0_ADD_N = 294,
     LOWER_THAN_ELSE = 295
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 52 "m1.y"

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



/* Line 1676 of yacc.c  */
#line 107 "m1parser.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


