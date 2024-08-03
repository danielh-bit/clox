#ifndef clox_debug_h
#define clox_debug_h

#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);
// returns new offset because instructions can have different sizes
int disassembleInstruction(Chunk* chunk, int offset);

#endif