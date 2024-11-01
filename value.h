#ifndef clox_value_h
#define clox_value_h

#include <string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

// the 64th bit.
#define SIGN_BIT ((uint64_t)0x8000000000000000)
// all the exponent, 'quite' and the intel bits.
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL   1 // 01.
#define TAG_FALSE 2 // 10.
#define TAG_TRUE  3 // 11.

typedef uint64_t Value;

// if all the exponent, 'quite' and the intel bits are on, it is not a number.
#define IS_NIL(value)       ((value) == NIL_VAL)
// this is to not use value twice (i.e v == true || v == false). when using value twice,
// if it is a function as input, the function will be called twice and that will be bad.
// this works the same without using value more than once.
#define IS_BOOL(value)      (((value) | 1) == TRUE_VAL)
#define IS_TRUE(value)      ((value) == TAG_TRUE)
#define IS_FALSE(value)     ((value) == TAG_FALSE)
#define IS_NUMBER(value)    (((value) & QNAN) != QNAN)
#define IS_OBJ(value) \
    (((value) & (SIGN_BIT | QNAN)) == (SIGN_BIT | QNAN))

#define AS_BOOL(value)      ((value) == TRUE_VAL) // as bool can be either true or false, if not true, it is false.
#define AS_NUMBER(value)    valueToNum(value)
#define AS_OBJ(value) \
    ((Obj*) (uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

#define NIL_VAL         ((Value)(uint64_t) (QNAN | TAG_NIL))
#define TRUE_VAL        ((Value)(uint64_t) (QNAN | TAG_TRUE))
#define FALSE_VAL       ((Value)(uint64_t) (QNAN | TAG_FALSE))
#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj) \
    (Value) (SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

// macros use memcpy because compiler assumes that double and uint64_t cannot be aliased to one another.
// memcpy is not slow because compiler optimizes the copying away.
static inline double valueToNum(Value value) {
    double num;
    memcpy(&num, &value, sizeof(Value));
    return num;
}

static inline Value numToValue(double num) {
    Value value;
    memcpy(&value, &num, sizeof(double));
    return value;
}

#else

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj* obj;
    } as;
} Value;

#define IS_BOOL(value)      ((value).type == VAL_BOOL)
#define IS_NIL(value)       ((value).type == VAL_NIL)
#define IS_NUMBER(value)    ((value).type == VAL_NUMBER)
#define IS_OBJ(value)       ((value).type == VAL_OBJ)

// turns Value into bool
#define AS_OBJ(value)       ((value).as.obj)
#define AS_BOOL(value)      ((value).as.boolean)
#define AS_NUMBER(value)    ((value).as.number)

// turns a bool into Value
// when you give a fuck, check why this works this way.
#define BOOL_VAL(value)     ((Value) {VAL_BOOL, {.boolean = value}})
#define NIL_VAL             ((Value) {VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value)   ((Value) {VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)     ((Value){VAL_OBJ, {.obj = (Obj*)object}})

#endif

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif