/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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
     KW_STRING = 262,
     TK_INT = 263,
     KW_STRUCT = 264,
     TK_INC = 265,
     TK_DEC = 266,
     KW_IF = 267,
     KW_WHILE = 268,
     KW_FOR = 269,
     KW_ELSE = 270,
     KW_DO = 271,
     TK_AND = 272,
     TK_OR = 273,
     TK_ARROW = 274,
     KW_VOID = 275,
     KW_NULL = 276,
     KW_BREAK = 277,
     KW_RETURN = 278,
     KW_CONST = 279,
     TK_GE = 280,
     TK_GT = 281,
     TK_LT = 282,
     TK_LE = 283,
     TK_EQ = 284,
     TK_NE = 285,
     KW_NAMESPACE = 286,
     TK_NS_SEP = 287,
     TK_LSH = 288,
     TK_RSH = 289,
     TK_STRING_CONST = 290,
     TK_INC_ASSIGN = 291,
     TK_DEC_ASSIGN = 292,
     KW_CASE = 293,
     KW_DEFAULT = 294,
     KW_SWITCH = 295,
     KW_PRINT = 296,
     KW_M0 = 297,
     TK_NL = 298,
     M0_NUMBER = 299,
     KW_ADD_I = 300,
     KW_ADD_N = 301,
     LOWER_THAN_ELSE = 302
   };
#endif
/* Tokens.  */
#define TK_IDENT 258
#define TK_NUMBER 259
#define KW_NUM 260
#define KW_INT 261
#define KW_STRING 262
#define TK_INT 263
#define KW_STRUCT 264
#define TK_INC 265
#define TK_DEC 266
#define KW_IF 267
#define KW_WHILE 268
#define KW_FOR 269
#define KW_ELSE 270
#define KW_DO 271
#define TK_AND 272
#define TK_OR 273
#define TK_ARROW 274
#define KW_VOID 275
#define KW_NULL 276
#define KW_BREAK 277
#define KW_RETURN 278
#define KW_CONST 279
#define TK_GE 280
#define TK_GT 281
#define TK_LT 282
#define TK_LE 283
#define TK_EQ 284
#define TK_NE 285
#define KW_NAMESPACE 286
#define TK_NS_SEP 287
#define TK_LSH 288
#define TK_RSH 289
#define TK_STRING_CONST 290
#define TK_INC_ASSIGN 291
#define TK_DEC_ASSIGN 292
#define KW_CASE 293
#define KW_DEFAULT 294
#define KW_SWITCH 295
#define KW_PRINT 296
#define KW_M0 297
#define TK_NL 298
#define M0_NUMBER 299
#define KW_ADD_I 300
#define KW_ADD_N 301
#define LOWER_THAN_ELSE 302




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 79 "m1.y"
{
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
/* Line 1529 of yacc.c.  */
#line 157 "m1parser.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



