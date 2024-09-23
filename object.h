#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value)     (AS_OBJ(value)->type)

#define IS_FUNCTION(value)  isObjType(value, OBJ_FUNCTION)
// this is in a separate function.
#define IS_STRING(value)    isObjType(value, OBJ_STRING)

// take a pointer to a valid string obj and convert it to obj.
#define AS_FUNCTION(value)  ((ObjFunction*) AS_OBJ(value))
#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))
// take a pointer to a valid string obj and convert it to a string in c (char *).
#define AS_CSTRING(value)   (((ObjString*)AS_OBJ(value))->chars)

typedef enum {
    OBJ_FUNCTION,
    OBJ_STRING,
} ObjType;

struct Obj {
  ObjType type;
  struct Obj* next; // this is to free all objects later.
};

typedef struct {
    Obj obj;
    int arity;
    Chunk chunk;
    ObjString* name;
} ObjFunction;

struct ObjString {
    Obj obj;
    int length;
    char* chars;
    // we add the hash into the string object because copying the chars of the string
    // takes O(n), creating the hash takes the same O(n), so it is efficient to eagerly
    // create the hash.
    uint32_t hash;
};

ObjFunction* newFunction();
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

// we use a separate function in the macro because value is used twice.
// if value was a function, (i.e: 'IS_STRING(pop())) then the function 
// will be called twice.
static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif