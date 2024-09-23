#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
} Local;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
} FunctionType;

typedef struct {
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

Parser parser;
// because BORING!
Compiler* current = NULL;

int startJump = -1; // not in a loop marking

static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode)
        return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Erorr", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // nothing   
    } else {
        fprintf(stderr, " at '%*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();

        if (parser.current.type != TOKEN_ERROR)
            break;
        
        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }
        
    errorAtCurrent(message);
}

static bool check(TokenType type) {
  return parser.current.type == type;
}

// returns if match, advances if true.
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static int emitLoop(uint8_t loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;

    if (offset > UINT16_MAX)
        error("Loop body too large.");
    
    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

// returns origin location for patching the jump.
static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    // place holder is 2 bytes.
    emitBytes(0xff, 0xff);

    return currentChunk()->count - 2;
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
    // TODO: make it work with long constants here.
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk");
        return 0;
    }

    return (uint8_t) constant;
}

static void emitConstant(Value value) {
    // TODO: make this work with long constants.
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
    // -2 to adjust for the bytecode of the jump offset placeholder.
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX)
        error("Too much code to jump over.");

    currentChunk()->code[offset] = (jump >> 8) & 0xff; // top 8 (shift rotates around so need mask)
    currentChunk()->code[offset + 1] = jump & 0xff; // bottom 8
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    // this looks dumb but is related to garbage collection.
    compiler->function = newFunction();
    current = compiler;

    // allocate stack position 0 for the main function.
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->name.start = "";
    local->name.length = 0;
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    // remove all locals in scope.
    while (current->localCount > 0 && current->locals[current->localCount - 1].depth > current->scopeDepth) {
        // this could be optimized by adding an OP_POPN
        // this OP will take an operand that will define
        // how by how much to move the stack pointer back
        // (poping multiple slots at once)
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) 
        return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

// this returns the index of the local in the locals array. this index
// matches the stack index of that variable
static int resolveLocal(Compiler* compiler, Token* name) {
    // from the top to make shadowing work correctly.
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            // -1 depth marks a variable that wasn't initialized yet. meaning it is refrencing it self.
            if (local->depth == -1)
                error("Can't read local variable in its own initializer");
            
            return i;
        }
    }

    return -1;
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function");
        return;
    }

    // we are incrementing the count after indexing and assigning
    Local* local = &current->locals[current->localCount++];
    local->name = name;
    // -1 marks the variable as still unintiallized (this is for some weird edge case).
    local->depth = -1;
}

static void declareVariable() {
    if (current->scopeDepth == 0)
        return;

    Token* name = &parser.previous;
    // erorr if 2 variables with same name in the same scope.
    // from the top because the new ones are in the same scope
    for(int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        // the local is not in the current scope or invalid
        if (local->depth != -1 && local->depth < current->scopeDepth)
            break;

        if (identifiersEqual(name, &local->name))
            error("Already a variable with this name in this scope.");
    }

    addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0)
        return 0; // return dummy index, because this works differently for locals.

    return identifierConstant(&parser.previous);
}

static void markInitialized() {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
    // means it is not global, thus not defined here
    if (current->scopeDepth > 0) {
        // remove -1 marking from var.
        markInitialized();
        return;
    }
    
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static void and_(bool canAssign) {
    // at calling the left hand side has been compiled already
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    // if left side is true then right side will decide the truthyness of the expression.
    // maybe better to add an instruction that pops and jumps.
    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    // this is to compile the right operand
    // make sure that it has lower precedence than the current expression. (this is why '+ 1')
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        // because we save instructions.
        case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        default: return; // Unreachable.
    }
}

static void literal(bool canAssign) {
    switch(parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: return; // uncreachable
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while(!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expected '}' after block.");
}

static void varDeclaration() {
    // adds and gets the index in constant table. (for the string name of var)
    uint8_t global = parseVariable("Expect variable name");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after value");
    // this emits the byte code
    defineVariable(global);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value");
    // this is to keep stack effect at zero.
    // all expressions have a stack effect of 1.
    emitByte(OP_POP);
}

#define MAX_CASES 256

static void switchStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after value.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before switch cases.");

    int prevCaseJump = 0;
    // 0 nothing, 1 in case, 2 in default.
    int state = 0;
    int ends[MAX_CASES];
    int caseCount = 0;

    while(!match(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        TokenType caseType = parser.current.type;

        if (caseType == TOKEN_CASE) {
            if (state == 2)
                error("Can't have a case after default case.");
            
            if (state == 1) {
                // add a jump to the end of the switch case.
                ends[caseCount] = emitJump(OP_JUMP);
                caseCount++;

                // patch the prevJump to next case (this).
                patchJump(prevCaseJump);
                emitByte(OP_POP); // pop result of comparison.
            }

            // jump over 'case'.
            advance();
            state = 1;
            emitByte(OP_DUP);
            expression();

            consume(TOKEN_COLON, "Expect ':' after case value.");

            emitByte(OP_EQUAL);
            prevCaseJump = emitJump(OP_JUMP_IF_FALSE);

            // pop comparison result
            emitByte(OP_POP);
            statement();
        } else if (caseType == TOKEN_DEFAULT) {
            if (state == 2)
                error("Can't another default.");

            if (state == 1) {
                ends[caseCount] = emitJump(OP_JUMP);
                caseCount++;

                emitByte(OP_POP); // comparison.
            }

            patchJump(prevCaseJump);

            advance();
            state = 2;
            // caseCount+=;
            consume(TOKEN_COLON, "Expect ':' after default.");

            statement();
        } else {
            if (state == 0) {
                error("Cannot have statements outside of cases.");
                return;
            }

            statement();
        }
    }

    if (state == 1) { // if ended on case, need to clear the jump that was made.
        patchJump(prevCaseJump);
        emitByte(OP_POP); // comparison
    }

    for (int i = 0; i < caseCount; i++) {
        patchJump(ends[i]);
    }

    emitByte(OP_POP); // the duped switch value
}

static void forStatement() {
    //> for-begin-scope
  beginScope();
//< for-begin-scope
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
/* Jumping Back and Forth for-statement < Jumping Back and Forth for-initializer
  consume(TOKEN_SEMICOLON, "Expect ';'.");
*/
//> for-initializer
  if (match(TOKEN_SEMICOLON)) {
    // No initializer.
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }
//< for-initializer

  int loopStart = currentChunk()->count;
  startJump = loopStart;
/* Jumping Back and Forth for-statement < Jumping Back and Forth for-exit
  consume(TOKEN_SEMICOLON, "Expect ';'.");
*/
//> for-exit
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    // Jump out of the loop if the condition is false.
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // Condition.
  }

//< for-exit
/* Jumping Back and Forth for-statement < Jumping Back and Forth for-increment
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
*/
//> for-increment
  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    emitLoop(loopStart);
    loopStart = incrementStart;
    startJump = loopStart;
    patchJump(bodyJump);
  }
//< for-increment

  statement();
  emitLoop(loopStart);
//> exit-jump

  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP); // Condition.
  }

//< exit-jump
//> for-end-scope
  endScope();
  startJump = -1; // for continue marking
//< for-end-scope
}

static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition");

    // this will emit the byte code for jump OP and a placeholder operand offset
    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    // this is for poping the condition. the pop is here instead of vm because logical
    // operators use jumps as well and don't pop.
    emitByte(OP_POP);
    statement();

    int elseJump = emitJump(OP_JUMP);

    // this will use the amount of slots that the previous statment took to replace the
    // placeholder value for jump OP.
    patchJump(thenJump);
    // pop the condition here if jumped false.
    emitByte(OP_POP);

    if (match(TOKEN_ELSE))
        statement();
    
    patchJump(elseJump);
}

// compares input value to NIL, enters if not
static void existsStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'exists'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition");

    emitConstant(NIL_VAL);
    // we check for not-equal.
    emitBytes(OP_EQUAL, OP_NOT);

    int endJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // comparison
    statement();

    int elseJump = emitJump(OP_JUMP);
    patchJump(endJump);
    emitByte(OP_POP); // comparison

    if (match(TOKEN_ELSE))
        statement();
    
    patchJump(elseJump);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value");
    emitByte(OP_PRINT);
}

static void continueStatement() {
    if (startJump == -1)
        error("Cannot perform a 'contine' outside of a loop");
    
    consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'");

    // add jump to start of loop.
    emitLoop(startJump);
}

static void whileStatement() {
    int loopStart = currentChunk()->count;
    startJump = loopStart;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();
    // this jumps backwards instead of forwards.
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
    startJump = -1;
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON)
            return;

        switch (parser.current.type) {
            // don't have semicolon
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:
                // nothing
        }

        advance();
    }
}

static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode)
        synchronize();
}

// has to have stack effect of zero.
static void statement() {
    // ugly else if thing because need match().
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_EXISTS)) {
        existsStatement();
    } else if (match(TOKEN_CONTINUE)) {
        continueStatement();
    } else if (match(TOKEN_SWITCH)) {
        switchStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression");
}

static void number(bool canAssign) {
    // this is a place where the Value typedef could break.
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
    // this entire thing is better with a separated OR instruction, but he is an indulger.
    // if left side is false, it is up to the right side to decide truethiness of expression.
    // this is to jump over the other jump (bruh).
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    // if left iside is true, the expression is true.
    int endJump = emitJump(OP_JUMP);

    // jumping over true case.
    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

static void string(bool canAssign) {
    // if string escape sequences were added to lox it would be here (i.e: '\n')

    // need to trim the leading and trailing quotation marks.
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                 parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    // this is local
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else { // global
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // compile operand.
    parsePrecedence(PREC_UNARY);

    switch (operatorType) {
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        case TOKEN_BANG: emitByte(OP_NOT); break;
        default: return; // uncreachable.
    }
}

ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,      PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,      PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,      PREC_NONE}, 
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,      PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,      PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,      PREC_NONE},
  [TOKEN_MINUS]         = {unary,    binary,    PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary,    PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,      PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary,    PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary,    PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,      PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary,    PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,      PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary,    PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary,    PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary,    PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary,    PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary,    PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,      PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,      PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,      PREC_NONE},
  [TOKEN_AND]           = {NULL,     and_,      PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,      PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,      PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,      PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,      PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,      PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,      PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,      PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,       PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,      PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,      PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,      PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,      PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,      PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,      PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,      PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,      PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,      PREC_NONE},
};  

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;

    if (prefixRule == NULL) {
        error("Expected expression.");
        return;
    }

    // this is for making sure that variable assignment is correct.
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    // a valid assignment would have already consumed TOKEN_EQUAL
    if (canAssign && match(TOKEN_EQUAL)) {
        error("invalid error assignment.");
    }
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    // remember to reset panic mode when statements are added.
    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}