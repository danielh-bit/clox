#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    vm.bytesAllocated += newSize - oldSize;
    if (newSize > oldSize) { // realloc can be used to free, we don't want to triger gc when freeing.
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif
    }

    if (vm.bytesAllocated > vm.nextGC) {
        collectGarbage();
    }

    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    
    if (result == NULL)
        exit(1);
    return result;
}

void markObject(Obj* object) {
    if (object == NULL) 
        return;
    if (object->isMarked)
        return;

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)object);
    // printValue(OBJ_VAL(object));
    printf("\n");
#endif
    
    object->isMarked = true;

    // grow gray stack if needed
    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        // we use native `realloc()` instead of `reallocate()` wrapper, to not trigger a recursive garbage collection.
        vm.grayStack = (Obj**) realloc(vm.grayStack, (sizeof(Obj*) * vm.grayCapacity));

        // sad
        if (vm.grayStack == NULL) 
            exit(1);
    }
    
    // add object to stack and increment stack pointer.
    vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value) {
    if (IS_OBJ(value))
        markObject(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)object);
    // printValue(OBJ_VAL(object));
    printf("\n");
#endif

    switch (object->type) {
        case OBJ_BOUND_METHOD: { // technically this is covered by the objClass method table, but scary to rely on. 
            ObjBoundMethod* bound = (ObjBoundMethod*) object;
            markValue(bound->reciever);
            markObject((Obj*) bound->method);
            break;
        }
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*) object;
            markObject((Obj*)klass->name);
            markTable(&klass->methods);
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*) object;
            markObject((Obj*) instance->klass);
            markTable(&instance->fields);
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*) object;
            markObject((Obj*) closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*) object;
            markObject((Obj*)function->name);
            markArray(&function->chunk.constants);
            break;
        }
        case OBJ_UPVALUE:
            markValue(((ObjUpvalue*)object)->closed);
            break;
        case OBJ_NATIVE:
        case OBJ_STRING:
            break; // these objects have no outgoing refrences. (could just darken them immediately)
    }
}

static void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p free for type %d, obj: ", (void*)object, object->type);
//   printObject(OBJ_VAL(object));
  printf("\n");
#endif

    switch(object->type) {
        case OBJ_BOUND_METHOD: {
            FREE(ObjBoundMethod, object);
            break;
        }
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*) object;
            freeTable(&klass->methods);
            FREE(ObjClass, object);
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*) object;
            freeTable(&instance->fields); // garbage collector will free entries to table.
            FREE(ObjInstance, object);
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*) closure;
            // printf("arity: %d\n", closure->upvalueCount);
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            // we don't free the function because the closure doesn't own the function
            // multiple closures can own the same function.
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)object;
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        case OBJ_NATIVE: {
            FREE(ObjNative, object);
            break;
        }
        case OBJ_STRING: {
            ObjString* string = (ObjString*) object;
            // printf("%s", string->chars);
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        case OBJ_UPVALUE: {
            FREE(ObjUpvalue, object);
            break;
        }
    }
}

static void markRoots() {
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    for (int i = 0; i < vm.frameCount; i++) {
        markObject((Obj*)vm.frames[i].closure);
    }

    for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    markTable(&vm.globals);
    markCompilerRoots();
}

static void traceRefrences() {
    while (vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

static void sweep() {
    Obj* previous = NULL;
    Obj* object = vm.objects;

    while (object != NULL) {
        if (object->isMarked) { // if reacheble don't free
            object->isMarked = false; // reset marking for next time.
            previous = object;
            object = object->next;
        } else {
            Obj* unreached = object;
            object = object->next;

            if (previous != NULL) { // unlink object (make the previous object skip it)
                previous->next = object;
            } else { // if the object is the head, change the head to the next object
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

    markRoots();
    traceRefrences();
    // need to remove the strings that were interned and are unreachable. they cannot be roots because all will be marked.
    // they are refrenced and then marked. now the strings that were not marked need to be deleted.
    tableRemoveWhite(&vm.strings);
    // printf("HAPPENS AFTER THIS\n");
    sweep();

    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");
  printf("  collect %zu bytes (from %zu to %zu) next at %zu", before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}

void freeObjects() {
    Obj* object = vm.objects;

    while (object != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.grayStack);
}
 