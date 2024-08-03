#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char* argv[]) {
    Chunk chunk;
    initChunk(&chunk);

    // this is not the constant but in fact the offset to the constant.
    writeConstant(&chunk, 1.2, 123);

    writeChunk(&chunk, OP_RETURN, 123);
    writeChunk(&chunk, OP_RETURN, 123);
    writeChunk(&chunk, OP_RETURN, 124);

    disassembleChunk(&chunk, "test chunk");
    freeChunk(&chunk);
    
    return 0;
}