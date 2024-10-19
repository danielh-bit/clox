#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

VM vm;

// this is the native function that has been created.
static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double) clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
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

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1; // -1 to point to previous failed instruction

        fprintf(stderr, "[line %d] in ", getLine(&function->chunk, instruction));
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

static void defineNative(const char* name, NativeFn function) {
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    // garbage collection stuff
    pop();
    pop();
}

void initVM() {
    resetStack();
    vm.objects = NULL;

    vm.grayCapacity = 0;
    vm.grayCount = 0;
    vm.grayStack = NULL;
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    initTable(&vm.globals);
    initTable(&vm.strings);

    defineNative("clock", clockNative);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    freeObjects();
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

// mutates the call frame stack to initialize new callee at the top
static bool call(ObjClosure* closure, int argCount) {
    if (closure->function->arity != argCount){
        runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow"); // the meme.
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    // jump the stack back so that the arguments are within the new call frame, and the function
    // is stored at stack index 0.
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_CLASS: {
                ObjClass* klass = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
                return true;
            }
            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE:
                NativeFn native = AS_NATIVE(callee);
                Value result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            default:
                break;
        }
    }
    runtimeError("Can only call function and classes.");
    return false;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues; // this is a linked list.
    // we start at the end of the upvalue linked list, the end will point to the upvalue
    // that is closest to the top of the stack. because of this, we know that when the location
    // of the upvalue is before the local, then it is not previously captured.
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local)
        return upvalue;

    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue; // set HEAD for linked list
    } else {
        prevUpvalue->next = createdUpvalue; // add to linked list.
    }

    return createdUpvalue;
}

static void closeUpvalues(Value* last) {
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        // now the upvalue is not stored on the stack. Thus, it needs to keep in on the heap (the ObjUpvalue).
        // to do this, it saves the Value in the closed field. and makes location point to that field to make 
        // the rest of the code still work.
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static bool isFalsy(Value value) {
    // this is fucking evil. need to think about 'nil' bit manipulation with 'AS_BOOL'
    return IS_NIL(value) || (IS_BOOL(value)) && !(AS_BOOL(value));
}

static void concatanate() {
    ObjString* b = AS_STRING(peek(0)); // for gc stuff
    ObjString* a = AS_STRING(peek(1));

    int length = a->length + b->length;
    // '+ 1' for null terminted string
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';
    
    ObjString* result = takeString(chars, length);
    pop(); // gc
    pop();
    push(OBJ_VAL(result));
}

Value longConstantInstruction(int offset) {
    uint32_t constant = (uint32_t) vm.chunk->code[offset] |
                        (uint32_t) vm.chunk->code[offset + 1] << 8 |
                        (uint32_t) vm.chunk->code[offset + 2] << 16;
    printf("%d\n", offset);
    return vm.chunk->constants.values[constant];
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | (frame->ip[-1])))
#define READ_STRING() AS_STRING(READ_CONSTANT())
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
        disassembleInstruction(&frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code));
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
            case OP_POP: pop(); break;
            case OP_DUP: push(peek(0)); break;
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;

                if (!tableGet(&vm.globals, name, &value)) { // if found tableGet will mutate value.
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // push mutated value from tableGet()
                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                                
                // this means that globals can be overwritten.
                tableSet(&vm.globals, name, peek(0));

                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();

                if (tableSet(&vm.globals, name, peek(0))) { // means this is an undefined var
                    // delete if undefined.
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }
            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(0));
                ObjString* name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    pop(); // pop instance
                    push(value);
                    break;
                }

                runtimeError("Undefined property '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(1));
                // we don't care that the instance does not contain the field, we will just add it as null
                tableSet(&instance->fields, READ_STRING(), peek(0));
                // this stack juglling is to remove the second element (instance).
                Value value = pop();
                pop(); // instance
                push(value);
                break;
            }
            case OP_EQUAL:
                Value b = pop();
                Value a = pop();

                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            case OP_GREATER:    BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:       BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD:
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatanate();
                } else if(IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    // we don't use BINARY_OP() because we need the costume runtime
                    // error message.
                    double b = AS_NUMBER(pop());
                    double a = AS_NUMBER(pop());

                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
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
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();

                if (isFalsy(peek(0)))
                    frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();

                // callValue mutates the callFrame stack
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1]; // new frame count after mutation from callValue

                break;
            }
            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);
                push(OBJ_VAL(closure));

                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        // pass the current slot we want to capture.
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        // the current function in frame is the surrounding one.
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }

                break;
            }
            case OP_CLOSE_UPVALUE: {
                closeUpvalues(vm.stackTop - 1);
                pop(); // remove the upvalue from the stack. (he is on top)
                break;
            }
            case OP_RETURN: {
                Value result = pop(); // we pop this because the entire stack window will be discarded
                closeUpvalues(frame->slots);
                vm.frameCount--;
                if(vm.frameCount == 0) {
                    pop(); // pop main (script)
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots; // discard stack window. (return to the start of the stack in the current frame)
                push(result); // push the return value (will be on top of the new stack)
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLASS: {
                push(OBJ_VAL(newClass((READ_STRING()))));
                break;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (function == NULL)
        return INTERPRET_COMPIPLE_ERROR;
    
    push(OBJ_VAL(function));
    ObjClosure* closure = newClosure(function); // main function
    pop();  // garbage collection stuff
    push(OBJ_VAL(closure));
    call(closure, 0); // setup implicit main function.

    return run();
}