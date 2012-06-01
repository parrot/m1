/*

Simple stack implementation.
Needed by code generator to store labels for break statements, etc.

*/
#include <stdio.h>
#include <stdlib.h>
#include "m1_stack.h"


m1_intstack *
new_stack(void) {
    m1_intstack *stack = (m1_intstack *)calloc(1, sizeof(m1_intstack));
    stack->sp          = 0;
    return stack;
}

void 
delete_stack(m1_intstack *stack) {
    free(stack);
    stack = NULL;
}

void 
push(m1_intstack *stack, int value) {
    stack->store[stack->sp++] = value;
}

int 
pop(m1_intstack *stack) {
    return stack->store[--stack->sp];
}

int 
top(m1_intstack *stack) {
    return stack->store[stack->sp - 1];    
}

m1_regstack *
new_regstack(void) {
    m1_regstack *stack = (m1_regstack *)calloc(1, sizeof(m1_regstack));
    stack->sp = 0;
    return stack;   
}

void
delete_regstack(m1_regstack *stack) {
    free(stack);
    stack = NULL;   
}

void
pushreg(m1_regstack *stack, m1_reg reg) {
    stack->store[stack->sp++] = reg;
}

m1_reg
popreg(m1_regstack *stack) {
    return stack->store[--stack->sp];   
}

m1_reg
topreg(m1_regstack *stack) {
    return stack->store[stack->sp - 1];   
}



