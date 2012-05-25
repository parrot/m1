#ifndef __M1_STACK_H__
#define __M1_STACK_H__

/* use a union if we need a stack for different types than int */
/*
union m1_stack_entry {
    int ival;
        
} m1_stack_entry;
*/

/* XXX make this flexible later */
#define STACKSIZE   128

typedef struct m1_stack {
    int store[STACKSIZE];
    int sp; /* stack pointer */        
} m1_stack;


extern m1_stack *new_stack(void);

extern void delete_stack(m1_stack *stack);

extern void push(m1_stack *stack, int value);

extern int pop(m1_stack *stack);

extern int top(m1_stack *stack);

#endif

