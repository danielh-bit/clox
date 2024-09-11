#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

void initTable(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table* table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key)  {
    uint32_t index = key->hash % capacity;
    Entry* tombstone = NULL;

    // loop until find free or appropriate (same key) entry.
    // note that the same key entry will always be after the intial index (wrap around - after)
    // because to get to a free entry the loop will go over all the occupied entries first.
    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (IS_NIL(entries->value)) {
                // empty entry that is available. return the tombstone if it was found before.
                // return entry if no tombstones were met before. this is for reusing tombstones.
                return tombstone != NULL ? tombstone : entry;
            } else {
                if (tombstone == NULL) // tobstone found
                    tombstone = entry;
            }
        } else if (entry->key == key) { // '==' works because of string interning
            return entry;
        }
        
        // the modulo is to make sure that the index "wraps around" if reached the end.
        index = (index + 1) % capacity;
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0)
        return false;
    
    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry == NULL)
        return false;
    
    *value = entry->value;
    return true;
}

// remakes the table with the new size.
static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);

    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // this is to reset the tombstones (they are counted).
    // this could mean that growing the capacity might reduce
    // the count. shouldnt be much of a problem though.
    table->count = 0;
    // mutate all entries from table and map them to their new destination.
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL)
            continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

// this is either a set or an add
bool tableSet(Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    // find either same key or empty bucket.
    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewkey = entry->key == NULL;

    if (isNewkey && IS_NIL(entry->value)) // we dont want to increment for tombstones (value == BOOL(true))
        table->count++;

    entry->key = key;
    entry->value = value;
    return isNewkey;
}

// we don't decrement the table count to not risk full array of tombstones and an 
// infinite loop in findEntry().
bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0)
        return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);
    if (entry->key == NULL)  
        return false;

    entry->key = NULL;
    entry->value = BOOL_VAL(true);
    return true;
}

void tableAddAll(Table* from, Table* to) {
    for (int i = 0; to->capacity; i++) {
        Entry* entry = &to->entries[i];
        if (entry != NULL)
            tableSet(to, entry->key, entry->value);
    }
}

ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table-> count == 0)
        return NULL;
    
    uint32_t index = hash % table->capacity;
    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            // stop if find non-tombstone, empty entry
            if (IS_NIL(entry->value))
                return NULL;
        } else if (entry->key->length == length && entry->key->hash == hash &&
            memcmp(entry->key->chars, chars, length) == 0) {
                // found
                return entry->key;
        }
        
        index = (index + 1) % table->capacity;
    }
}