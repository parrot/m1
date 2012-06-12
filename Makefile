CC      = gcc
LEX     = flex
YACC    = bison
DEBUG   = true

O       = .o
EXE     =
TEMPDIR = /tmp

ifeq ($(DEBUG), true)
CFLAGS = -O0 -g -W -Wall
else
CFLAGS = -O3 -W -Wall
endif

M1_O_FILES = \
	src/m1parser$(O) \
	src/m1lexer$(O) \
	src/ast$(O) \
	src/symtab$(O) \
	src/semcheck$(O) \
	src/stack$(O) \
	src/decl$(O) \
	src/eval$(O) \
	src/instr$(O) \
	src/gencode$(O) \
	src/main$(O) \

m1$(EXE): $(M1_O_FILES)
	$(CC) -I$(@D) -o m1$(EXE) $(M1_O_FILES)

src/m1lexer$(O): src/m1lexer.c
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1lexer.c

src/m1parser$(O): src/m1parser.c
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/m1parser.c	

src/m1parser.c: src/m1.y src/m1lexer.c
	$(YACC) --output=src/m1parser.c src/m1.y

src/m1lexer.c: src/m1.l
	$(LEX) --header-file=src/m1lexer.h --outfile=src/m1lexer.c src/m1.l	
	
src/ast$(O): src/ast.c src/ast.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/ast.c

src/eval$(O): src/eval.c src/eval.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/eval.c	

src/symtab$(O): src/symtab.c src/symtab.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/symtab.c

src/instr$(O): src/instr.c src/instr.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/instr.c

src/gencode$(O): src/gencode.c src/gencode.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/gencode.c

src/semcheck$(O): src/semcheck.c src/semcheck.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/semcheck.c

src/stack$(O): src/stack.c src/stack.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/stack.c

src/main$(O): src/m1parser.h src/main.c
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/main.c

src/decl$(O): src/decl.c src/decl.h
	$(CC) $(CFLAGS) -I$(@D) -o $@ -c src/decl.c

test-v: m1$(EXE)
	prove -r -v --ext .m1 --exec ./run_m1.sh t/

test: m1$(EXE)
	prove -r --ext .m1 --exec ./run_m1.sh t/

clean:
	$(RM) -rf src/m1parser.* \
		src/m1lexer.* \
		src/*$(O) \
		./m1$(EXE) \
		t/*.m0*
# For checking with splint see also
# http://trac.parrot.org/parrot/wiki/splint
# Splint: http://splint.org
SPLINT = splint

# Temp directory for splint.  Add +keep to splintflags if you want work files
# kept in there after completion.
SPLINT_TMP = $(TEMPDIR)/splint

# Splint flags: http://splint.org/manual/html/appB.html
# The dashes in the names don't make any difference to Splint, but I've
# made them match how they are in the manual.  Otherwise, you might be
# looking for "declundef", except that it's "decl-undef" in the docs.
SPLINTFLAGS_BASE = \
	+weak \
	+hints \
	\
	+indentspaces 4 \
	+locindentspaces 4 \
	+linelen 999 \
	+bugslimit 1000 \
	-message-stream-stdout \
	+showdeephistory \
	+show-func \
	+show-column \
	-tmpdir $(SPLINT_TMP) \
	\
	+posix-lib \
	-skip-posix-headers \
	\
	+ansi89-limits \
	+num-struct-fields 255 \
	how-summary \
	+show-scan \
	+time-dist \
	\
	+cpp-names \
	+ansi-reserved \
	+ansi-reserved-internal \
	-iso-reserved \
	-iso-reserved-internal \
	-include-nest 10 \

SPLINTFLAGS_NULL_DEREFERENCES = \
	+null \
	+nullret \

SPLINTFLAGS_USE_BEFORE_DEFINITION = \
	+usedef \
	+incondefs \
	+functionderef \

SPLINTFLAGS_TYPE = \
	+string-literal-too-long \
	+string-literal-no-room \
	+string-literal-no-room-final-null \
	+string-literal-smaller \
	+enum-members \
	\
	-pred-bool \
	-pred-bool-ptr \
	-pred-bool-int \
	+pred-bool-others \
	+pred-assign \
	+ptrnegate \
	-zero-ptr \
	\
	+charunsignedchar \
	+char-index \
	+char-int \
	\
	+format-code \
	+format-type \
	\
	+ignore-signs \
	+long-unsigned-unsigned-integral \
SPLINTFLAGS_MEMORY_MANAGEMENT = \

SPLINTFLAGS_SHARING = \

SPLINTFLAGS_FUNCTION_INTERFACE = \

# Macro safety checks
SPLINTFLAGS_MACRO = \
	+macro-assign \
	+macro-empty \
	+macro-parens \
	+macro-redef \
	+macro-stmt \
	+macro-unrecog \

SPLINTFLAGS_NAMING = \

SPLINTFLAGS_CONTROL_FLOW = \
	+eval-order \
	+eval-order-uncon \

SPLINTFLAGS_MEMORY_BOUNDS = \

SPLINTFLAGS_COMPLETENESS = \
SPLINTFLAGS_MISCELLANEOUS = \

# Other options we'd like to add back
# +initallelements : Right now, the *.ops files don't initialize all
#  values of the arrays
# +casebreak: Auto-generated ops have way too case fallthrus right now
# +fcnuse: We have many functions that are defined but not used, but they
#  should get hidden or ifdeffed
# +redef, +redecl: Ops currently have tons of redefinitions

# added to splint target to simplify experimentation,
# example: make SPLINTFLAGS_TEST='-posixstrictlib +posixlib' splint
SPLINTFLAGS_TEST =

SPLINTFLAGS = \
	$(SPLINTFLAGS_BASE) \
	$(SPLINTFLAGS_NULL_DEREFERENCES) \
	$(SPLINTFLAGS_USE_BEFORE_DEFINITION) \
	$(SPLINTFLAGS_TYPE) \
	$(SPLINTFLAGS_MEMORY_MANAGEMENT) \
	$(SPLINTFLAGS_SHARING) \
	$(SPLINTFLAGS_FUNCTION_INTERFACE) \
	$(SPLINTFLAGS_MACRO) \
	$(SPLINTFLAGS_NAMING) \
	$(SPLINTFLAGS_CONTROL_FLOW) \
	$(SPLINTFLAGS_MEMORY_BOUNDS) \
	$(SPLINTFLAGS_COMPLETENESS) \
	$(SPLINTFLAGS_MISCELLANEOUS) \

SPLINT_SOURCE = \
	$$(find src/*.c -type f | \
	sort)

# "splint" is the less-thorough splint target.  For cage cleaning work,
# you'll probably want to specify SPLINT_SOURCE rather than work on the
# entire tree, like so:
#     make splint SPLINT_SOURCE='src/*.c'
splint :
	$(SPLINT) "-I./src" $(SPLINTFLAGS) \
	+partial -DNDEBUG \
	$(SPLINT_SOURCE) \
		| grep -v 'Source code error generation point'
