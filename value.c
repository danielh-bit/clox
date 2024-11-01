#include <stdio.h>
#include <string.h>

#include "object.h"
#include "memory.h"
#include "value.h"
#include "vm.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    pop();
    if(array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
#ifdef NAN_BOXING
    if (IS_BOOL(value)) {
        printf(AS_BOOL(value) ? "true" : "false");
    } else if (IS_NIL(value)) {
        printf("nil");
    } else if (IS_NUMBER(value)) {
        printf("%g", AS_NUMBER(value));
    } else if (IS_OBJ(value)) {
        printObject(value);
    }
#else
    switch(value.type) {
        case VAL_BOOL:
            printf(AS_BOOL(value) ? "true": "false");
            break;
        case VAL_NIL: printf("nil"); break;
        case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;
        case VAL_OBJ: printObject(value); break;
    }
#endif
}

// PHP considers the strings “1” and “01” to be 
// equivalent because both can be converted to equivalent 
// numbers, though the ultimate reason is because PHP was 
// designed by a Lovecraftian eldritch god to destroy the mind.

// also cant compare the unions directly because padding can cause weird shit.
// thats kinda funny.

bool valuesEqual(Value a, Value b) {
#ifdef NAN_BOXING
    // this is because NAN values don't equal to themselves for some reason.
    // because the values are now uint64_t and not double, we need to convert them
    // to comply with the IEEE 754 spec.
    // if (IS_NUMBER(a) && IS_NUMBER(b)) {
    //     return AS_NUMBER(a) == AS_NUMBER(b);     this is commented because fuck the IEEE 754 spec.
    // }
    return a == b; // crazy!
#else
    if (a.type != b.type)
        return false;
    
    // a.type == b.type, so this checks for both
    switch(a.type) {
        case VAL_BOOL:  return AS_BOOL(a) == AS_BOOL(b);
        case VAL_NIL:   return true;
        case VAL_NUMBER:return AS_NUMBER(a) == AS_NUMBER(b);
        case VAL_OBJ:   return AS_OBJ(a) == AS_OBJ(b);
        default: return false; // unreachable.
    }
#endif
}