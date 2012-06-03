/*

Simple stack implementation.
Needed by code generator to store labels for break statements, etc.

*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "stack.h"

#define STACKDEBUG  1

m1_intstack *
new_intstack(void) {
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
    assert(stack != NULL);
    assert(stack->sp < STACKSIZE);
    
    stack->store[stack->sp++] = value;
}

int 
pop(m1_intstack *stack) {
    assert(stack != NULL);
    assert(stack->sp > 0);
    
    return stack->store[--stack->sp];
}

int 
top(m1_intstack *stack) {
    assert(stack != NULL);
    assert(stack->sp > 0);
    
    return stack->store[stack->sp - 1];    
}

m1_regstack *
new_regstack(void) {
    m1_regstack *stack = (m1_regstack *)calloc(1, sizeof(m1_regstack));
    stack->sp = 0;
    return stack;   
}

void
print_stack(m1_regstack *stack, char *message) {
    assert(stack != NULL);
    assert(message != NULL);
#if STACKDEBUG     
    int i = stack->sp - 1;
    static const char r[4] = {'I', 'N', 'S', 'P'};
   
    fprintf(stderr, "[printstack] %s\n", message);
    while (i >= 0) {
        fprintf(stderr, "%d | %c%d |\n", i, r[(int)stack->store[i].type], stack->store[i].no);
        --i;   
    }    
    fprintf(stderr, "--------\n");
#endif    
}

void
delete_regstack(m1_regstack *stack) {
    assert(stack != NULL);
    free(stack);
    stack = NULL;   
}

void
pushreg(m1_regstack *stack, m1_reg reg) {
    assert(stack != NULL);
    assert(stack->sp < STACKSIZE);
    
    stack->store[stack->sp++] = reg;
    print_stack(stack, "push (after)");
}

m1_reg
popreg(m1_regstack *stack) {
    m1_reg r;
    
    assert(stack != NULL);
    assert(stack->sp > 0);
    
    r = stack->store[--stack->sp];   
    print_stack(stack, "pop (after)");
    return r;
}

m1_reg
topreg(m1_regstack *stack) {
    m1_reg r;
    
    assert(stack != NULL);
    assert(stack->sp > 0);
    
    r = stack->store[stack->sp - 1];   
    print_stack(stack, "top (after)");
    return r;
}



