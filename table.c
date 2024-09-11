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

    // loop until find free or appropriate (same key) entry.
    // note that the same key entry will always be after the intial index (wrap around - after)
    // because to get to a free entry the loop will go over all the occupied entries first.
    for (;;) {
        Entry* entry = &entries[index];
        // found entry. why '==' works?
        if (entry->key == key || entry->key == NULL) {
            // return null keyed entry if new bucket.
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

static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);

    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = NIL_VAL;
    }

    // mutate all entries from table and map them to their new destination.
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL)
            continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool tableSet(Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    // find either same key or empty bucket.
    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewkey = entry->key == NULL;

    if (isNewkey)
        table->count++;

    entry->key = key;
    entry->value = value;
    return isNewkey;
}

void tableAddAll(Table* from, Table* to) {
    for (int i = 0; to->capacity; i++) {
        Entry* entry = &to->entries[i];
        if (entry != NULL)
            tableSet(to, entry->key, entry->value);
    }
}