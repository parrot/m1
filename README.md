m1
==

low-level C-family language that spits out M0

Overview
========
M

Running
-------


Language grammar
================


Implementation
==============
M1 is implemented in C. The parser is implemented using Bison, whereas the lexer is
implemented using Flex. The compiler consists of the following modules

* Lexer (m1.l)
* Parser (m1.y) 
* Abstract Syntax Tree nodes (m1_ast.c,h)
* Code generator (m1_codegen.c,h)

Other files include:

* Compiler definition (m1_compiler.h)
* Source code annotations for splint (m1_ann.h)


Abstract Syntax Tree
--------------------
During the parsing phase, the parser invokes AST node constructors. Most expressions and statements
are represented by m1_expression nodes. Keeping the number of node types limited simplifies the AST,
which makes it easier to understand the compiler.

Code generator
--------------
The code generator walks the AST, and generates instructions for each node, if applicable. Most
functions in the code generator return a m1_reg structure, which represents a register, containing
the register number and register type. The m1_reg structure that is returned contains the register
that will hold the result of the evaluation of the AST node. 