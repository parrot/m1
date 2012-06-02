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
     TK_SCOPE = 287,
     TK_LSH = 288,
     TK_RSH = 289,
     TK_STRING_CONST = 290,
     TK_INC_ASSIGN = 291,
     TK_DEC_ASSIGN = 292,
     TK_MUL_ASSIGN = 293,
     TK_DIV_ASSIGN = 294,
     TK_MOD_ASSIGN = 295,
     TK_SHL_ASSIGN = 296,
     TK_SHR_ASSIGN = 297,
     TK_BAND_ASSIGN = 298,
     TK_BOR_ASSIGN = 299,
     KW_CASE = 300,
     KW_DEFAULT = 301,
     KW_SWITCH = 302,
     KW_PRINT = 303,
     KW_PMC = 304,
     KW_EXTENDS = 305,
     KW_VTABLE = 306,
     KW_METHOD = 307,
     KW_NEW = 308,
     KW_SUPER = 309,
     KW_SELF = 310,
     KW_FALSE = 311,
     KW_TRUE = 312,
     TK_ISTRUE = 313,
     TK_NOT = 314,
     KW_EXTERN = 315,
     KW_IMPORT = 316,
     KW_UNSIGNED = 317,
     KW_BOOL = 318,
     KW_CATCH = 319,
     KW_THROW = 320,
     KW_TRY = 321,
     KW_CONTINUE = 322,
     KW_INLINE = 323,
     KW_PRIVATE = 324,
     KW_PUBLIC = 325,
     KW_ENUM = 326,
     KW_M0 = 327,
     TK_NL = 328,
     M0_NUMBER = 329,
     KW_NOOP = 330,
     KW_GOTO = 331,
     KW_GOTO_IF = 332,
     KW_GOTO_CHUNK = 333,
     KW_ADD_I = 334,
     KW_ADD_N = 335,
     KW_SUB_I = 336,
     KW_SUB_N = 337,
     KW_MULT_I = 338,
     KW_MULT_N = 339,
     KW_DIV_I = 340,
     KW_DIV_N = 341,
     KW_MOD_I = 342,
     KW_MOD_N = 343,
     KW_ITON = 344,
     KW_NTOI = 345,
     KW_ASHR = 346,
     KW_LSHR = 347,
     KW_SHL = 348,
     KW_AND = 349,
     KW_OR = 350,
     KW_XOR = 351,
     KW_GC_ALLOC = 352,
     KW_SYS_ALLOC = 353,
     KW_SYS_FREE = 354,
     KW_COPY_MEM = 355,
     KW_SET = 356,
     KW_SET_IMM = 357,
     KW_DEREF = 358,
     KW_SET_REF = 359,
     KW_SET_BYTE = 360,
     KW_GET_BYTE = 361,
     KW_SET_WORD = 362,
     KW_GET_WORD = 363,
     KW_CSYM = 364,
     KW_CCALL_ARG = 365,
     KW_CCALL_RET = 366,
     KW_CCALL = 367,
     KW_PRINT_S = 368,
     KW_PRINT_I = 369,
     KW_PRINT_N = 370,
     KW_EXIT = 371,
     TK_USERTYPE = 372,
     LOWER_THAN_ELSE = 373
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
#define TK_SCOPE 287
#define TK_LSH 288
#define TK_RSH 289
#define TK_STRING_CONST 290
#define TK_INC_ASSIGN 291
#define TK_DEC_ASSIGN 292
#define TK_MUL_ASSIGN 293
#define TK_DIV_ASSIGN 294
#define TK_MOD_ASSIGN 295
#define TK_SHL_ASSIGN 296
#define TK_SHR_ASSIGN 297
#define TK_BAND_ASSIGN 298
#define TK_BOR_ASSIGN 299
#define KW_CASE 300
#define KW_DEFAULT 301
#define KW_SWITCH 302
#define KW_PRINT 303
#define KW_PMC 304
#define KW_EXTENDS 305
#define KW_VTABLE 306
#define KW_METHOD 307
#define KW_NEW 308
#define KW_SUPER 309
#define KW_SELF 310
#define KW_FALSE 311
#define KW_TRUE 312
#define TK_ISTRUE 313
#define TK_NOT 314
#define KW_EXTERN 315
#define KW_IMPORT 316
#define KW_UNSIGNED 317
#define KW_BOOL 318
#define KW_CATCH 319
#define KW_THROW 320
#define KW_TRY 321
#define KW_CONTINUE 322
#define KW_INLINE 323
#define KW_PRIVATE 324
#define KW_PUBLIC 325
#define KW_ENUM 326
#define KW_M0 327
#define TK_NL 328
#define M0_NUMBER 329
#define KW_NOOP 330
#define KW_GOTO 331
#define KW_GOTO_IF 332
#define KW_GOTO_CHUNK 333
#define KW_ADD_I 334
#define KW_ADD_N 335
#define KW_SUB_I 336
#define KW_SUB_N 337
#define KW_MULT_I 338
#define KW_MULT_N 339
#define KW_DIV_I 340
#define KW_DIV_N 341
#define KW_MOD_I 342
#define KW_MOD_N 343
#define KW_ITON 344
#define KW_NTOI 345
#define KW_ASHR 346
#define KW_LSHR 347
#define KW_SHL 348
#define KW_AND 349
#define KW_OR 350
#define KW_XOR 351
#define KW_GC_ALLOC 352
#define KW_SYS_ALLOC 353
#define KW_SYS_FREE 354
#define KW_COPY_MEM 355
#define KW_SET 356
#define KW_SET_IMM 357
#define KW_DEREF 358
#define KW_SET_REF 359
#define KW_SET_BYTE 360
#define KW_GET_BYTE 361
#define KW_SET_WORD 362
#define KW_GET_WORD 363
#define KW_CSYM 364
#define KW_CCALL_ARG 365
#define KW_CCALL_RET 366
#define KW_CCALL 367
#define KW_PRINT_S 368
#define KW_PRINT_I 369
#define KW_PRINT_N 370
#define KW_EXIT 371
#define TK_USERTYPE 372
#define LOWER_THAN_ELSE 373




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 37 "src/m1.y"
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
    struct m1_case			*cse;
}
/* Line 1529 of yacc.c.  */
#line 300 "m1parser.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



