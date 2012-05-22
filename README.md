m1
==

low-level C-family language that spits out M0

Overview
========





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

