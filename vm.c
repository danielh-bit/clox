#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
}

// lol the lines don't work.
// variadic function...
static void runtimeError(const char* format, ...) {
    // this is the stuff that lets us pass arbitrary amount of arguments
    va_list args;
    va_start(args, format);
    // variadic 'printf'
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    // this is the instruction position. '- 1' because the vm.ip is one after the current instruction.
    size_t instruction = vm.ip - vm.chunk->code - 1;
    // the instructions offset should currespond with the correct line of for that instruction.
    // beacuse i done his challenge i need to use this function
    int line = getLine(vm.chunk, instruction);
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
}

void initVM() {
    resetStack();
}

void freeVM() {

}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    // vm.stackTop--;
    // return *vm.stackTop;
    // this should do the same I tink.
    return *(--vm.stackTop);
}

static Value peek(int distance) {
    // the '-1' is because the stackTop is one after the current value.
    // distance is negated because it can only go back.
    return vm.stackTop[-1 - distance];
}

static bool isFalsy(Value value) {
    // this is fucking evil. need to think about 'nil' bit manipulation with 'AS_BOOL'
    return IS_NIL(value) || (IS_BOOL(value)) && !(AS_BOOL(value));
}

Value longConstantInstruction(int offset) {
    uint32_t constant = (uint32_t) vm.chunk->code[offset] |
                        (uint32_t) vm.chunk->code[offset + 1] << 8 |
                        (uint32_t) vm.chunk->code[offset + 2] << 16;
    printf("%d\n", offset);
    return vm.chunk->constants.values[constant];
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
// burf imoji (or maybe genius).
#define BINARY_OP(valueType, op) \
    do { \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runtimeError("Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = AS_NUMBER(pop()); \
        double a = AS_NUMBER(pop()); \
        push(valueType(a op b)); \
    } while(false)

    for (;;) {
        // this is for debugging while creating the VM. needs to be disabled in "common.h" when user
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;
        switch(instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            } //push(-pop());
            case OP_NIL: push(NIL_VAL); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_EQUAL:
                Value b = pop();
                Value a = pop();

                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            case OP_GREATER:    BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:       BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD:        BINARY_OP(NUMBER_VAL, +); break;
            case OP_SUBTRACT:   BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY:   BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:     BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT:
                push(BOOL_VAL(isFalsy(pop())));
                break;
            case OP_NEGATE:     
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPIPLE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;

    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
}