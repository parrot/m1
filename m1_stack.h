#ifndef __M1_STACK_H__
#define __M1_STACK_H__

#include "m1_gencode.h"

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


extern m1_intstack *new_stack(void);

extern void delete_stack(m1_intstack *stack);

extern void push(m1_intstack *stack, int value);

extern int pop(m1_intstack *stack);

extern int top(m1_intstack *stack);

#endif

