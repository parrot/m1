M1
==

low-level C-family language that spits out [M0](https://github.com/parrot/parrot/tree/m0)

Overview
========
M1 was defined as a C-like language but much simplified. C is a very large language, and its syntax
allows to implement many types of hacks. The goal of M1 is to implement a useful subset, extended
with specific functionality (and syntax) that is useful to implement Parrot. As a result of being
a restricted subset of C, the language forces to use a "standardized" way of using the language.

Ideas for future features
=========================
* Built-in splint-like checks
* Built-in coding standards checks (resulting in syntax errors if not adhered to)


Running
-------
After downloading the source code, just type "make". Then invoke ./m1 <m1 script>.

Language grammar
================

Insert M1 grammar here.


Implementation
--------------
M1 is implemented in C. The parser is implemented using Bison, whereas the lexer is
implemented using Flex. The compiler consists of the following modules

* Lexer (m1.l)
* Parser (m1.y) 
* Abstract Syntax Tree nodes (m1_ast.c,h)
* Code generator (m1_codegen.c,h)

Other files include:

* Compiler definition (m1_compiler.h)
* Source code annotations for splint (m1_ann.h)

The M1 compiler is reentrant; the parser and lexer that are generated are therefore also
reentrant. {{note: a few things to fix for this}}.


Abstract Syntax Tree
====================
During the parsing phase, the parser invokes AST node constructors. Most expressions and statements
are represented by m1_expression nodes. Keeping the number of node types limited simplifies the AST,
which makes it easier to understand the compiler.

Code generator
==============
The code generator walks the AST, and generates instructions for each node, if applicable. Most
functions in the code generator return a m1_reg structure, which represents a register, containing
the register number and register type. The m1_reg structure that is returned contains the register
that will hold the result of the evaluation of the AST node. 

