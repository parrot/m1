#include <stdio.h>
#include <stdlib.h>
#include "m1_stack.h"


m1_stack *
new_stack(void) {
    m1_stack *stack = (m1_stack *)calloc(1, sizeof(m1_stack));
    stack->sp       = 0;
    return stack;
}

void 
delete_stack(m1_stack *stack) {
    
}

void 
push(m1_stack *stack, int value) {
    stack->store[stack->sp++] = value;
}

int 
pop(m1_stack *stack) {
    return stack->store[--stack->sp];
}

int 
top(m1_stack *stack) {
    return stack->store[stack->sp - 1];    
}

