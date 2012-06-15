#ifndef __M1_STACK_H__
#define __M1_STACK_H__

#include "gencode.h"

/* XXX make this flexible later */
#define STACKSIZE   128

typedef struct m1_intstack {
    int store[STACKSIZE];
    int sp; /* stack pointer */        
    
} m1_intstack;


typedef struct m1_regstack {
    struct m1_reg store[STACKSIZE];
    int           sp; /* stack pointer */
    
} m1_regstack;


extern m1_intstack *new_intstack(void);

extern void delete_stack(m1_intstack *stack);

extern void push(m1_intstack *stack, int value);

extern int pop(m1_intstack *stack);

extern int top(m1_intstack *stack);

extern m1_regstack *new_regstack(void);

extern void delete_regstack(m1_regstack *stack);

extern void pushreg(m1_regstack *stack, m1_reg reg);

extern m1_reg popreg(m1_regstack *stack);

extern m1_reg topreg(m1_regstack *stack);

extern int intstack_isempty(m1_intstack *stack) ;
extern int regstack_isempty(m1_regstack *stack) ;

extern void print_stack(m1_regstack *stack, char *message);

#endif

