#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include "tinytemplate.h"

/* Configurations */

// When enabled, the compiler dumps the location of 
// the calls to [next_token] and [peek_token] to 
// stderr. This is very useful when debugging.
//
// #define TINYTEMPLATE_TRACE_TOKENS

/* Some definitions to help with readability  */

#define DONE    TINYTEMPLATE_STATUS_DONE
#define ESYMBOL TINYTEMPLATE_STATUS_ESYMBOL
#define ESCOPE  TINYTEMPLATE_STATUS_ESCOPE
#define EDEPTH  TINYTEMPLATE_STATUS_EDEPTH
#define ETYPE   TINYTEMPLATE_STATUS_ETYPE
#define EITER   TINYTEMPLATE_STATUS_EITER
#define EMEMORY TINYTEMPLATE_STATUS_EMEMORY
#define ESYNTAX TINYTEMPLATE_STATUS_ESYNTAX
#define ESEMANT TINYTEMPLATE_STATUS_ESEMANT

#define instr_t  tinytemplate_instr_t
#define status_t tinytemplate_status_t

/* Utilities */

#define NOT_IMPLEMENTED_YET assert(0)

/*
    NOPE  - No effect
    DONE  - Conclude execution
    PUSHI - Push integer on the evaluation stack
    PUSHF - Push float on the evaluation stack
    PUSHS - Push string on the evaluation stack
    PUSHV - Push value of a variable on the evaluation stack
    JUMP  - Jump to a given instruction of the program
    JCND  - Jump to a given instruction of the program if a condition is verified
    WRITE - Write to output a string
    WRTOP - Write to output the top of the evaluation stack
    POP   - Pop a value from the evaluation stack
    ADD   - Pop the top 2 value of the evaluation stack and push their sum
    SUB   - Same as ADD, but with subtraction
    MUL   - Same as ADD, but with multiplication
    DIV   - Same as ADD, but with division
    MOD   - Same as ADD, but with the remainder of division
*/

typedef enum {
    OPCODE_NOPE,
    OPCODE_DONE,
    OPCODE_PUSHI,
    OPCODE_PUSHF,
    OPCODE_PUSHS,
    OPCODE_PUSHV,
    OPCODE_JUMP,
    OPCODE_JCND,
    OPCODE_WRITE,
    OPCODE_WRTOP,
    OPCODE_POP,
    OPCODE_ADD,
    OPCODE_SUB,
    OPCODE_MUL,
    OPCODE_DIV,
    OPCODE_MOD,
    OPCODE_NEG,
    OPCODE_GETS,
    OPCODE_ITER,
    OPCODE_NEXT,
    OPCODE_CHLD,
    OPCODE_IDX,
} opcode_t;

// Represents a substring of the template
typedef struct {
    size_t offset;
    size_t length;
} slice_t;

typedef enum {
    SCOPE_IF,
    SCOPE_IF_ELSE,
    SCOPE_FOR,
} scope_type_t;

typedef struct {
    scope_type_t type;

    size_t if_jcnd;
    size_t if_jump;
    
    size_t  for_next;
    slice_t for_child_label;
    slice_t for_index_label;

} scope_t;

typedef struct {
    scope_t scope_stack[TINYTEMPLATE_MAX_SCOPE_DEPTH];
    size_t  scope_depth;

    instr_t *program;
    size_t num_instr;
    size_t max_instr;

    bool failed;
} compile_state_t;

typedef struct {
    const char *src;
    size_t cur, len;
} scanner_t;

typedef union {
    int64_t as_int;
    double  as_float;
} token_payload_t;

typedef enum {
    TOKEN_OPER_ADD = '+', // These are defined to avoid
    TOKEN_OPER_SUB = '-', // compiler warnings. All ASCII
    TOKEN_OPER_MUL = '*', // values are assumed to be
    TOKEN_OPER_DIV = '/', // valid "token_t"s.

    TOKEN_END = 128,
    TOKEN_IDENT,
    TOKEN_NONASCII,
    TOKEN_NONPRINT,
    TOKEN_VALUE_INT,
    TOKEN_VALUE_FLOAT,
    TOKEN_KWORD_IF,
    TOKEN_KWORD_IN,
    TOKEN_KWORD_FOR,
    TOKEN_KWORD_ELSE,
    TOKEN_KWORD_END,
    TOKEN_OPER_MOD,
} token_t;

typedef struct {
    size_t max;
    char  *dst;
} error_t;

static void report(error_t *error, const char *fmt, ...)
{
    if (error->dst) {
        va_list args;
        va_start(args, fmt);
        vsnprintf(error->dst, error->max, fmt, args);
        va_end(args);
    }
}

static void 
append_instr(compile_state_t *state, 
             opcode_t opcode, ...)
{
    if (state->failed)
        return;

    if (state->num_instr == state->max_instr) {
        state->failed = true;
        return;
    }

    va_list operands;
    va_start(operands, opcode);

    tinytemplate_instr_t *instr = &state->program[state->num_instr++];

    instr->opcode = opcode;
    switch (opcode) {
        
        case OPCODE_ITER:
        case OPCODE_CHLD:
        case OPCODE_IDX:
        instr->operands[0].as_size = va_arg(operands, size_t); 
        break;

        case OPCODE_NEXT:
        instr->operands[0].as_size = va_arg(operands, size_t); 
        break;

        case OPCODE_NOPE:
        case OPCODE_DONE:
        break;

        case OPCODE_PUSHI: instr->operands[0].as_int   = va_arg(operands, int64_t); break;
        case OPCODE_PUSHF: instr->operands[0].as_float = va_arg(operands, double);  break;
        
        case OPCODE_GETS:
        case OPCODE_PUSHV:
        case OPCODE_PUSHS: 
        instr->operands[0].as_size = va_arg(operands, size_t); 
        instr->operands[1].as_size = va_arg(operands, size_t); 
        break;

        case OPCODE_JUMP:
        case OPCODE_JCND:
        instr->operands[0].as_size = va_arg(operands, size_t);
        break;
        
        case OPCODE_WRITE:
        instr->operands[0].as_size = va_arg(operands, size_t); 
        instr->operands[1].as_size = va_arg(operands, size_t); 
        break;

        case OPCODE_WRTOP:
        case OPCODE_POP:
        case OPCODE_ADD:
        case OPCODE_SUB:
        case OPCODE_MUL:
        case OPCODE_DIV:
        case OPCODE_MOD:
        case OPCODE_NEG:
        break;
    }

    va_end(operands);
}

static bool is_alpha(char c)
{
    return (c >= 'a' && c <= 'z') 
        || (c >= 'A' && c <= 'Z');
}

static bool is_digit(char c)
{
    return c >= '0' && c <= '9';
}

static bool is_space(char c)
{
    return c == ' '  || c == '\t'
        || c == '\r' || c == '\n';
}

static bool is_ascii(char c)
{
    return !((unsigned char) c & (1 << 7));
}

static bool is_printable(char c)
{
    return (unsigned char) c >= 32
        && (unsigned char) c <= 126;
}

static bool
follows_digit(scanner_t *s)
{
    return s->cur < s->len && is_digit(s->src[s->cur]);
}

static bool
follows_alpha(scanner_t *s)
{
    return s->cur < s->len && is_alpha(s->src[s->cur]);
}

static bool
follows_space(scanner_t *s)
{
    return s->cur < s->len && is_space(s->src[s->cur]);
}

static bool 
follows_char(scanner_t *s, char c)
{
    return s->cur < s->len && s->src[s->cur] == c;
}

static bool 
follows_pair(scanner_t *s, char pair[static 2])
{
    return s->cur+1 < s->len 
        && s->src[s->cur+0] == pair[0] 
        && s->src[s->cur+1] == pair[1];
}

static bool 
consume_pair(scanner_t *s, char pair[static 2])
{
    bool ok = follows_pair(s, pair);
    if (ok) s->cur += 2;
    return ok;
}

static void 
consume_spaces(scanner_t *s)
{
    while (follows_space(s))
        s->cur++;
}

static token_t 
next_token_int(scanner_t *scanner, slice_t *slice,
               token_payload_t *payload)
{
    assert(follows_digit(scanner));
    
    size_t offset = scanner->cur;
    
    int64_t buf = 0;
    do {
        int d = scanner->src[scanner->cur++] - '0';
        if (buf > (INT64_MAX - d) / 10) {
            // Overflow!
            buf = INT64_MAX;
            break;
        }
        buf = buf * 10 + d;
    } while (follows_digit(scanner));

    if (slice) {
        slice->offset = offset;
        slice->length = scanner->cur - offset;
    }

    if (payload)
        payload->as_int = buf;
    return TOKEN_VALUE_INT;
}

static token_t 
next_token_float(scanner_t *scanner, slice_t *slice,
                 token_payload_t *payload)
{
    // The caller made sure that follows a float:
    // a string of digits followed by a dot and
    // another string of digits.
    assert(follows_digit(scanner));

    size_t offset = scanner->cur;

    double buf = 0;
    do {
        int d = scanner->src[scanner->cur++] - '0';
        buf = buf * 10 + d;
    } while (scanner->src[scanner->cur] != '.');

    scanner->cur++; // Skip the "."

    double q = 1;
    do {
        q /= 10;
        int d = scanner->src[scanner->cur++] - '0';
        buf += d * q;
    } while (follows_digit(scanner));

    if (slice) {
        slice->offset = offset;
        slice->length = scanner->cur - offset;
    }

    if (payload)
        payload->as_float = buf;
    return TOKEN_VALUE_FLOAT;
}

static token_t 
next_token_numeric(scanner_t *scanner, slice_t *slice,
                   token_payload_t *payload)
{
    // Scanner points to a digit
    assert(follows_digit(scanner));

    // Is it a float or an int? Check if at the end
    // of the first sequence of digit there's a dot
    // followed by a digit.
    size_t cur = scanner->cur;
    while (cur < scanner->len && is_digit(scanner->src[cur]))
        cur++;

    if (cur+1 < scanner->len && scanner->src[cur] == '.' && is_digit(scanner->src[cur+1]))
        return next_token_float(scanner, slice, payload);
    else
        return next_token_int(scanner, slice, payload);
}

static token_t
next_token_kword_or_ident(scanner_t *scanner, slice_t *slice,
                          token_payload_t *payload)
{
    (void) payload;

    assert(follows_alpha(scanner) || follows_char(scanner, '_'));

    size_t offset = scanner->cur;
    do
        scanner->cur++;
    while (follows_alpha(scanner) || follows_char(scanner, '_'));
    size_t length = scanner->cur - offset;

    if (slice) {
        slice->offset = offset;
        slice->length = length;
    }

    switch (length) {
        case 2: // if, in
        if (!strncmp(scanner->src + offset, "if", length))
            return TOKEN_KWORD_IF;
        if (!strncmp(scanner->src + offset, "in", length))
            return TOKEN_KWORD_IN;
        break;

        case 3: // end, mod, for
        if (!strncmp(scanner->src + offset, "end", length))
            return TOKEN_KWORD_END;
        if (!strncmp(scanner->src + offset, "mod", length))
            return TOKEN_OPER_MOD;
        if (!strncmp(scanner->src + offset, "for", length))
            return TOKEN_KWORD_FOR;
        break;

        case 4: // else
        if (!strncmp(scanner->src + offset, "else", length))
            return TOKEN_KWORD_ELSE;
        break;
    }

    return TOKEN_IDENT;
}

static token_t 
next_token(scanner_t *scanner, slice_t *slice,
           token_payload_t *payload)
{
    consume_spaces(scanner);

    if (scanner->cur == scanner->len) {
        if (slice) {
            slice->offset = scanner->cur;
            slice->length = 0;
        }
        return TOKEN_END;
    }

    char c = scanner->src[scanner->cur];

    if (is_digit(c))
        return next_token_numeric(scanner, slice, payload);

    if (is_alpha(c))
        return next_token_kword_or_ident(scanner, slice, payload);
    
    if (!is_ascii(c)) {
        size_t offset = scanner->cur;
        do
            scanner->cur++;
        while (scanner->cur < scanner->len && !is_ascii(scanner->src[scanner->cur]));
        if (slice) {
            slice->offset = offset;
            slice->length = scanner->cur - offset;
        }
        return TOKEN_NONASCII;
    }

    if (!is_printable(c)) {
        size_t offset = scanner->cur;
        do
            scanner->cur++;
        while (scanner->cur < scanner->len && !is_printable(scanner->src[scanner->cur]));
        if (slice) {
            slice->offset = offset;
            slice->length = scanner->cur - offset;
        }
        return TOKEN_NONPRINT;
    }

    size_t offset = scanner->cur;
    scanner->cur++;
    if (slice) {
        slice->offset = offset;
        slice->length = scanner->cur - offset;
    }
    return (token_t) c;
}

static token_t 
peek_token(scanner_t *scanner, 
           slice_t *slice,
           token_payload_t *payload)
{
    size_t cur = scanner->cur;
    token_t token = next_token(scanner, slice, payload);
    scanner->cur = cur;
    return token;
}

#ifdef TINYTEMPLATE_TRACE_TOKENS
static token_t 
trace_next_token(const char *c_func, const char *c_file, 
                 int c_line, scanner_t *scanner, slice_t *slice,
                 token_payload_t *payload)
{
    slice_t maybe;
    if (slice == NULL)
        slice = &maybe;
    token_t token = next_token(scanner, slice, payload);
    fprintf(stderr, "NEXT TOKEN [%.*s] @ %s in %s:%d\n", 
            (int) slice->length, scanner->src + slice->offset, 
            c_func, c_file, c_line);
    return token;
}

static token_t 
trace_peek_token(const char *c_func, const char *c_file, 
                 int c_line, scanner_t *scanner, slice_t *slice,
                 token_payload_t *payload)
{
    slice_t maybe;
    if (slice == NULL)
        slice = &maybe;
    token_t token = peek_token(scanner, slice, payload);
    fprintf(stderr, "PEEK TOKEN [%.*s] @ %s in %s:%d\n", 
            (int) slice->length, scanner->src + slice->offset, 
            c_func, c_file, c_line);
    return token;
}
#define next_token(scanner, slice, payload) trace_next_token(__func__, __FILE__, __LINE__, scanner, slice, payload)
#define peek_token(scanner, slice, payload) trace_peek_token(__func__, __FILE__, __LINE__, scanner, slice, payload)
#endif

static status_t
parse_primary(scanner_t *scanner, 
              compile_state_t *state,
              error_t *error)
{
    slice_t slice;
    token_payload_t payload;
    token_t token = next_token(scanner, &slice, &payload);

    switch (token) {
        
        case TOKEN_VALUE_INT:   append_instr(state, OPCODE_PUSHI, payload.as_int);   break;
        case TOKEN_VALUE_FLOAT: append_instr(state, OPCODE_PUSHF, payload.as_float); break;
        case TOKEN_IDENT:
        {
            // If the identifier refers to an iteration label,
            // than push it. Otherwise assume is a template
            // parameter provided by the caller program during
            // evaluation.
            bool found = false;
            for (size_t i = 0, j = 0; i < state->scope_depth; i++) {
                scope_t *scope = &state->scope_stack[state->scope_depth-i-1];
                if (scope->type == SCOPE_FOR) {

                    // Check if the label matches the iteration label
                    if (slice.length == scope->for_child_label.length && !memcmp(scanner->src + slice.offset, scanner->src + scope->for_child_label.offset, slice.length)) {
                        append_instr(state, OPCODE_CHLD, j);
                        found = true;
                        break;
                    }

                    // Check if the label matches the index label. 
                    // If no iteration label was specified, then
                    // its length will be 0 and the expression's
                    // label won't match.
                    if (slice.length == scope->for_index_label.length && !memcmp(scanner->src + slice.offset, scanner->src + scope->for_index_label.offset, slice.length)) {
                        append_instr(state, OPCODE_IDX, j);
                        found = true;
                        break;
                    }
                    j++;
                }
            }
            
            if (!found)
                // Label doesn't refer to an iteration
                append_instr(state, OPCODE_PUSHV, slice.offset, slice.length);
            break;
        }

        default:
        report(error, "Bad token [%.*s] in primary expression", 
               (int) slice.length, scanner->src + slice.offset);
        return ESYNTAX;
    }
    return DONE;
}

static status_t
parse_suffix(scanner_t *scanner,
             compile_state_t *state,
             error_t *error)
{
    status_t status;

    status = parse_primary(scanner, state, error);
    if (status != DONE)
        return status;

    token_t suffix = peek_token(scanner, NULL, NULL);
    if (suffix == '.') {

        // Offset of the dot token
        size_t checkpoint = scanner->cur;
        
        // Consume the dot
        next_token(scanner, NULL, NULL);

        slice_t slice;
        token_t key = next_token(scanner, &slice, NULL);
        if (key == TOKEN_IDENT)
            append_instr(state, OPCODE_GETS, slice.offset, slice.length);
        else
            scanner->cur = checkpoint;
    }
    return DONE;
}

static bool
parse_prefix(scanner_t *scanner,
             compile_state_t *state,
             error_t *error)
{
    status_t status;

    token_t prefix = peek_token(scanner, NULL, NULL);
    if (prefix == '+' || prefix == '-') {
        next_token(scanner, NULL, NULL);
        status = parse_prefix(scanner, state, error);
        if (status != DONE)
            return status;
        if (prefix == '-')
            append_instr(state, OPCODE_NEG);
        return DONE;
    }
    return parse_suffix(scanner, state, error);
}

static int 
precedence_of(token_t token)
{
    switch (token) {
        case '+': return 1;
        case '-': return 1;
        case '*': return 2;
        case '/': return 2;
        case TOKEN_OPER_MOD: return 2;
        default: break;
    }
    return 0;
}

static bool 
is_binary_operator(token_t token)
{
    return precedence_of(token) > 0;
}

static bool 
is_right_associative(token_t token)
{
    (void) token;
    return false;
}

static bool 
can_continue_climbing(scanner_t *scanner, 
                      int min_precedence)
{
    token_t peek = peek_token(scanner, NULL, NULL);
    return is_binary_operator(peek) && precedence_of(peek) >= min_precedence;
}

static bool 
should_associate_right(scanner_t *scanner, 
                       token_t oper, token_t *peek)
{
    *peek = peek_token(scanner, NULL, NULL);
    return is_binary_operator(*peek) 
        && (precedence_of(*peek) > precedence_of(oper) || (precedence_of(*peek) == precedence_of(oper) && is_right_associative(*peek)));
}

static opcode_t 
operator_opcode(token_t token)
{
    switch (token) {
        case '+': return OPCODE_ADD;
        case '-': return OPCODE_SUB;
        case '*': return OPCODE_MUL;
        case '/': return OPCODE_DIV;
        case TOKEN_OPER_MOD: return OPCODE_MOD;
        default: break;
    }

    return OPCODE_ADD;
}

static status_t
parse_expr_2(scanner_t *scanner, 
             compile_state_t *state,
             int min_precedence,
             error_t *error)
{
    status_t status;

    while (can_continue_climbing(scanner, min_precedence)) {
        
        token_t oper = next_token(scanner, NULL, NULL);
        if ((status = parse_prefix(scanner, state, error)) != DONE)
            return status;

        token_t peek;
        while (should_associate_right(scanner, oper, &peek)) {
            int precedence = precedence_of(oper) + (precedence_of(peek) > precedence_of(oper));
            if ((status = parse_expr_2(scanner, state, precedence, error)) != DONE)
                return status;
        }

        append_instr(state, operator_opcode(oper));
    }
    return DONE;
}

static bool 
parse_expr(scanner_t *scanner, 
           compile_state_t *state,
           error_t *error)
{
    status_t status;
    if ((status = parse_prefix(scanner, state, error)) != DONE)
        return status;
    return parse_expr_2(scanner, state, 0, error);
}

static status_t
expr_block(scanner_t *scanner, 
           compile_state_t *state,
           error_t *error)
{
    scanner->cur += 2; // Skip the "{{"
    
    status_t status = parse_expr(scanner, state, error);
    if (status != DONE)
        return status;

    append_instr(state, OPCODE_WRTOP);
    append_instr(state, OPCODE_POP, 1);

    if (!consume_pair(scanner, "}}")) {
        report(error, "No closing [}}] after expression block");
        return ESYNTAX;
    }
    return DONE;
}

static status_t
close_construct(scanner_t *scanner,
                error_t *error,
                const char *block_name)
{
    // Consume the following "%}"
    consume_spaces(scanner);
    if (!consume_pair(scanner, "%}")) {
        report(error, "Missing closing %%} in {%% %s %%} block", block_name);
        return ESYNTAX;
    }
    return DONE;
}

static status_t
selection_construct_start(scanner_t *scanner,
                          compile_state_t *state,
                          error_t *error)
{
    status_t status;

    // This function is called after "{% if" is parsed
    assert(scanner->src[scanner->cur-2] == 'i' 
        && scanner->src[scanner->cur-1] == 'f');

    if (state->scope_depth == TINYTEMPLATE_MAX_SCOPE_DEPTH) {
        report(error, "Scope depth limit reached");
        return ESCOPE;
    }

    if ((status = parse_expr(scanner, state, error)) != DONE)
        return status;

    if ((status = close_construct(scanner, error, "if")) != DONE)
        return status;
    
    size_t if_jcnd = state->num_instr;
    append_instr(state, OPCODE_JCND, 0);
    
    state->scope_stack[state->scope_depth].type = SCOPE_IF;
    state->scope_stack[state->scope_depth].if_jcnd = if_jcnd;
    state->scope_depth++;
    return DONE;
}

static status_t
iteration_construct_start(scanner_t *scanner,
                          compile_state_t *state,
                          error_t *error)
{
    // This function is called after "{% for" is parsed
    assert(scanner->src[scanner->cur-3] == 'f' 
        && scanner->src[scanner->cur-2] == 'o'
        && scanner->src[scanner->cur-1] == 'r');

    if (state->scope_depth == TINYTEMPLATE_MAX_SCOPE_DEPTH) {
        report(error, "Scope depth limit reached");
        return ESCOPE;
    }

    slice_t child_label;
    if (next_token(scanner, &child_label, NULL) != TOKEN_IDENT) {
        report(error, "Missing iteration label");
        return ESYNTAX;
    }

    slice_t index_label;
    if (peek_token(scanner, NULL, NULL) == ',') {
        next_token(scanner, NULL, NULL); // Consume the comma
        if (next_token(scanner, &index_label, NULL) != TOKEN_IDENT) {
            report(error, "Missing iteration index label after [,]");
            return ESYNTAX;
        }
    } else
        index_label = (slice_t) {0, 0};

    if (next_token(scanner, NULL, NULL) != TOKEN_KWORD_IN) {
        report(error, "Missing [in] keyword after iteration label");
        return ESYNTAX;
    }

    status_t status;
    if ((status = parse_expr(scanner, state, error)) != DONE)
        return status;
    
    append_instr(state, OPCODE_ITER);
    size_t for_next = state->num_instr;
    append_instr(state, OPCODE_NEXT, 0);

    if ((status = close_construct(scanner, error, "for")) != DONE)
        return status;

    state->scope_stack[state->scope_depth].type = SCOPE_FOR;
    state->scope_stack[state->scope_depth].for_child_label = child_label;
    state->scope_stack[state->scope_depth].for_index_label = index_label;
    state->scope_stack[state->scope_depth].for_next = for_next;
    state->scope_depth++;
    return DONE;
}

static void
resolve_scope(compile_state_t *state)
{
    assert(state->scope_depth > 0);

    scope_t *scope = state->scope_stack 
                   + state->scope_depth-1;

    switch (scope->type) {
        
        // Useful for all cases but it changes
        // meaning for each one.
        tinytemplate_instr_t *instr;

        case SCOPE_FOR:
        append_instr(state, OPCODE_JUMP, scope->for_next);
        instr = state->program + scope->for_next;
        instr->operands[0].as_size = state->num_instr;
        break;

        case SCOPE_IF:
        instr = state->program + scope->if_jcnd;
        instr->operands[0].as_size = state->num_instr;
        break;

        case SCOPE_IF_ELSE:
        instr = state->program + scope->if_jump;
        instr->operands[0].as_size = state->num_instr;
        break;
    }

    state->scope_depth--;
}

static status_t
construct_end(scanner_t *scanner,
              compile_state_t *state,
              error_t *error)
{
    if (state->scope_depth == 0) {
        report(error, "Orphan {%% end %%} block");
        return ESEMANT;
    }

    status_t status = close_construct(scanner, error, "end");
    if (status != DONE)
        return status;

    resolve_scope(state);
    return DONE;
}

static status_t
construct_else(scanner_t *scanner,
               compile_state_t *state,
               error_t *error)
{
    if (state->scope_depth == 0) {
        report(error, "Orphan {%% else %%} block");
        return ESEMANT;
    }

    status_t status = close_construct(scanner, error, "else");
    if (status != DONE)
        return status;

    scope_t *scope = state->scope_stack 
                   + state->scope_depth - 1;

    switch (scope->type) {
    
        tinytemplate_instr_t *instr;

        case SCOPE_IF:
        scope->if_jump = state->num_instr;
        append_instr(state, OPCODE_JUMP, 0);
        instr = state->program + scope->if_jcnd;
        instr->operands[0].as_size = state->num_instr;
        scope->type = SCOPE_IF_ELSE;
        break;

        case SCOPE_IF_ELSE:
        report(error, "Duplicate {%% else %%} case");
        return ESEMANT;

        case SCOPE_FOR:
        report(error, "Bad {%% else %%} coupled with {%% for .. %%}");
        return ESEMANT;
    }

    return DONE;
}

static status_t
control_flow_block(scanner_t *scanner, 
                   compile_state_t *state,
                   error_t *error)
{
    scanner->cur += 2; // Skip the "{%"

    slice_t slice;
    switch (next_token(scanner, &slice, NULL)) {
        case TOKEN_KWORD_IF:   return selection_construct_start(scanner, state, error);
        case TOKEN_KWORD_FOR:  return iteration_construct_start(scanner, state, error);
        case TOKEN_KWORD_END:  return construct_end(scanner, state, error);
        case TOKEN_KWORD_ELSE: return construct_else(scanner, state, error);
        default:
        report(error, "Bad token [%.*s] after [{%%]", 
               (int) slice.length, scanner->src + slice.offset);
        return ESYNTAX;
    }
    return DONE;
}

status_t 
tinytemplate_compile(const char *src, size_t len, 
                     instr_t *program, size_t max_instr,
                     size_t *num_instr, char *errmsg,
                     size_t errmax)
{
    error_t error = {
        .dst=errmsg,
        .max=errmax,
    };

    scanner_t scanner = {
        .src=src, 
        .len=len, 
        .cur=0
    };
    
    compile_state_t state = {
        .program=program, 
        .max_instr=max_instr, 
        .num_instr=0, 
        .failed=false
    };

    status_t status;

    while (scanner.cur < scanner.len) {
        // A program is a sequence of alternating raw text blocks and
        // blocks enclosed in "{%" and "%}" or "{{" and "}}". So for
        // each iteration we scan through a block of raw text and then
        // a "{{ .. }}"/"{% .. %}" block.

        size_t raw_off = scanner.cur; // Start offset of the raw block
        while (scanner.cur < scanner.len) {
            // Look for a "{" or the end
            while (scanner.cur < scanner.len && !follows_char(&scanner, '{'))
                scanner.cur++;

            // If the end wasn't reached (a "{" was found)
            // then exit if the following character is a
            // "{" or "%". If it isn't, then skip the first
            // "{" and continue scanning for the next "{"
            // by starting a new iteration.
            if (scanner.cur < scanner.len) {
                assert(scanner.src[scanner.cur] == '{');
                if (scanner.cur+1 < scanner.len && (scanner.src[scanner.cur+1] == '{' || scanner.src[scanner.cur+1] == '%'))
                    break;
                scanner.cur++; // Consume the "{"
            }
        }
        size_t raw_len = scanner.cur - raw_off; // Length of the raw block

        if (raw_len > 0) // The raw block isn't empty
            append_instr(&state, OPCODE_WRITE, raw_off, raw_len);

        if (scanner.cur < scanner.len) {
            assert(scanner.cur+1 < scanner.len && scanner.src[scanner.cur] == '{' && (scanner.src[scanner.cur+1] == '{' || scanner.src[scanner.cur+1] == '%'));

            if (scanner.src[scanner.cur+1] == '{')
                status = expr_block(&scanner, &state, &error);
            else {
                assert(scanner.src[scanner.cur+1] == '%');
                status = control_flow_block(&scanner, &state, &error);
            }
            if (status != DONE)
                return status;
        }
    }

    // Close all pending scoped that were
    // waiting for a {% end %} block.
    while (state.scope_depth > 0)
        resolve_scope(&state);

    append_instr(&state, OPCODE_DONE);

    if (state.failed) {
        report(&error, "Out of template program memory");
        return EMEMORY;
    }
    if (num_instr)
        *num_instr = state.num_instr;
    return DONE;
}

static bool
value_can_be_considered_true(tinytemplate_type_t type, 
                             tinytemplate_union_t data)
{
    switch (type) {

        case TINYTEMPLATE_TYPE_INT:
        return data.as_int != 0;

        case TINYTEMPLATE_TYPE_FLOAT:
        return data.as_float != 0;

        default:
        return false;
    }
}

typedef struct {
    tinytemplate_array_t iter;
    tinytemplate_value_t child;
    size_t next_index;
} iter_state_t;

status_t 
tinytemplate_eval(const char *src, const instr_t *program, 
                  void *userp, tinytemplate_getter_t params,
                  tinytemplate_callback_t callback, 
                  char *errmsg, size_t errmax)
{
    error_t error = {.dst=errmsg, .max=errmax};

    iter_state_t iter_stack[TINYTEMPLATE_MAX_ITER_DEPTH];
    size_t       iter_depth = 0;

    tinytemplate_type_t  types[TINYTEMPLATE_MAX_EXPR_DEPTH];
    tinytemplate_union_t stack[TINYTEMPLATE_MAX_EXPR_DEPTH];
    size_t stack_depth = 0;
    
    bool done = false;
    int index = 0;
    while (!done) {
        const tinytemplate_instr_t *instr = &program[index++];
        switch (instr->opcode) {
            
            case OPCODE_NOPE: 
            /* Do nothing */ 
            break;
            
            case OPCODE_DONE: 
            done = true; 
            break;

            case OPCODE_ITER:
            assert(stack_depth > 0); // ITER excepts a value on the stack
                                     // as an iteration target
            if (types[stack_depth-1] != TINYTEMPLATE_TYPE_ARRAY) {
                report(&error, "Iteration on something other than an array");
                return ETYPE;
            }
            if (iter_depth == TINYTEMPLATE_MAX_ITER_DEPTH) {
                report(&error, "Maximum nested iteration limit reached");
                return EITER;
            }
            iter_stack[iter_depth].iter = stack[--stack_depth].as_array;
            iter_stack[iter_depth].next_index = 0;
            iter_depth++;
            break;

            case OPCODE_NEXT:
            {
                assert(iter_depth > 0); // The NEXT instruction can't
                                        // be run outside of an iteration.

                iter_state_t *top_iter = &iter_stack[iter_depth-1];
                tinytemplate_value_t value;
                if (top_iter->iter.next(top_iter->iter.data, &value)) {
                    top_iter->child = value;
                    top_iter->next_index++;
                } else {
                    index = instr->operands[0].as_size;
                    iter_depth--;
                }
                break;
            }

            case OPCODE_CHLD:
            {
                if (stack_depth == TINYTEMPLATE_MAX_EXPR_DEPTH) {
                    report(&error, "Evaluation stack limit reached");
                    return EDEPTH;
                }

                size_t iter_idx = instr->operands[0].as_size;

                assert(iter_depth > 0); // The CHLD instruction can't
                                        // be run outside of an iteration.
                
                assert(iter_idx < iter_depth); // The index of the iteration must
                                               // refer to one of the active ones.

                iter_state_t *iter = &iter_stack[iter_depth-iter_idx-1];

                assert(iter->next_index > 0); // CHLD can only be executed after
                                              // the iteration started.

                stack[stack_depth] = iter->child.data;
                types[stack_depth] = iter->child.type;
                stack_depth++;
                break;
            }

            case OPCODE_IDX:
            {
                if (stack_depth == TINYTEMPLATE_MAX_EXPR_DEPTH) {
                    report(&error, "Evaluation stack limit reached");
                    return EDEPTH;
                }

                size_t iter_idx = instr->operands[0].as_size;

                assert(iter_depth > 0); // The IDX instruction can't
                                        // be run outside of an iteration.

                assert(iter_idx < iter_depth); // The index of the iteration must
                                               // refer to one of the active ones.

                iter_state_t *iter = &iter_stack[iter_depth-iter_idx-1];

                assert(iter->next_index > 0); // IDX can only be executed after
                                              // the iteration started.

                stack[stack_depth].as_int = iter->next_index-1;
                types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                stack_depth++;
                break;
            }

            case OPCODE_GETS:
            {
                assert(stack_depth > 0); // GETS takes the top of the stack
                                         // and transforms it in one of its
                                         // properties, so the stack can't
                                         // be empty.

                if (types[stack_depth-1] != TINYTEMPLATE_TYPE_DICT) {
                    report(&error, "Access by string on non-dict value");
                    return ETYPE;
                }
    
                size_t offset = instr->operands[0].as_size;
                size_t length = instr->operands[1].as_size;
                tinytemplate_value_t value;
                if (!stack[stack_depth-1].as_dict.get(stack[stack_depth-1].as_dict.data, src + offset, length, &value)) {
                    report(&error, "Key %.*s not in dict", (int) length, src + offset);
                    return ESYMBOL;
                }
                types[stack_depth-1] = value.type;
                stack[stack_depth-1] = value.data;
                break;
            }

            case OPCODE_PUSHI: 
            if (stack_depth == TINYTEMPLATE_MAX_EXPR_DEPTH) {
                report(&error, "Evaluation stack limit reached");
                return EDEPTH;
            }
            types[stack_depth] = TINYTEMPLATE_TYPE_INT;
            stack[stack_depth].as_int = instr->operands[0].as_int;
            stack_depth++;
            break;
            
            case OPCODE_PUSHF: 
            if (stack_depth == TINYTEMPLATE_MAX_EXPR_DEPTH) {
                report(&error, "Evaluation stack limit reached");
                return EDEPTH;
            }
            types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
            stack[stack_depth].as_float = instr->operands[0].as_float;
            stack_depth++;
            break;
            
            case OPCODE_PUSHS: NOT_IMPLEMENTED_YET; break;
            
            case OPCODE_PUSHV:
            {
                if (stack_depth == TINYTEMPLATE_MAX_EXPR_DEPTH) {
                    report(&error, "Evaluation stack limit reached");
                    return EDEPTH;
                }
                size_t varname_offset = instr->operands[0].as_size;
                size_t varname_length = instr->operands[1].as_size;

                tinytemplate_value_t value;
                if (params(userp, src + varname_offset, varname_length, &value)) {
                    types[stack_depth] = value.type;
                    stack[stack_depth] = value.data;
                    stack_depth++;
                } else {
                    report(&error, "Undefined variable [%.*s]", 
                           (int) varname_length, src + varname_offset);
                    return ESYMBOL;
                }
                break;
            }

            case OPCODE_JUMP:
            index = instr->operands[0].as_size;
            break;
            
            case OPCODE_JCND:
            assert(stack_depth > 0); // JCND jumps if the top of the
                                     // stack is true, so the stack
                                     // can't be empty.
            if (!value_can_be_considered_true(types[stack_depth-1], stack[stack_depth-1]))
                index = instr->operands[0].as_size;
            stack_depth--;
            break;
            
            case OPCODE_WRITE: 
            {
                size_t offset = instr->operands[0].as_size;
                size_t length = instr->operands[1].as_size;
                callback(userp, src + offset, length); 
                break;
            }

            case OPCODE_WRTOP:
            {
                assert(stack_depth > 0); // WRTOP output the top of the stack,
                                         // so the stack can't be empty.

                switch (types[stack_depth-1]) {
                    case TINYTEMPLATE_TYPE_INT:
                    {
                        char text[128];
                        int num = snprintf(text, sizeof(text), "%lld", stack[stack_depth-1].as_int);
                        assert(num > 0);
                        callback(userp, text, (size_t) num);
                        break;
                    }

                    case TINYTEMPLATE_TYPE_FLOAT: 
                    {
                        char text[128];
                        int num = snprintf(text, sizeof(text), "%lf", stack[stack_depth-1].as_float); 
                        assert(num > 0);
                        callback(userp, text, (size_t) num);
                        break;
                    }

                    case TINYTEMPLATE_TYPE_STRING:
                    callback(userp, stack[stack_depth-1].as_string.str, stack[stack_depth-1].as_string.len);
                    break;

                    default:
                    report(&error, "Can't write non-primitive value");
                    return ETYPE;
                }
                break;
            }
            
            case OPCODE_POP:
            assert(stack_depth > 0);
            stack_depth--;
            break;
            
            // This will be useful from here on
            #define PAIR(x, y) ((unsigned char) (x) | ((unsigned char) (y) << 8))

            case OPCODE_NEG:
            {
                assert(stack_depth > 0);

                switch (types[stack_depth-1]) {
                    
                    case TINYTEMPLATE_TYPE_INT:
                    stack[stack_depth-1].as_int = -stack[stack_depth-1].as_int;
                    break;
                    
                    case TINYTEMPLATE_TYPE_FLOAT:
                    stack[stack_depth-1].as_float = -stack[stack_depth-1].as_float;
                    break;

                    default:
                    report(&error, "Negation on non-numeric operands");
                    return ETYPE;
                }
                break;
            }

            case OPCODE_ADD:
            {
                assert(stack_depth >= 2);

                switch PAIR(types[stack_depth-2], types[stack_depth-1]) {
                
                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_INT): 
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = stack[stack_depth-1].as_int;
                        int64_t res = op1 + op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_FLOAT):
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = (int64_t) stack[stack_depth-1].as_float;
                        int64_t res = op1 + op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_FLOAT, 
                              TINYTEMPLATE_TYPE_INT):
                    {
                        double op1 = stack[stack_depth-2].as_float;
                        double op2 = (double) stack[stack_depth-1].as_int;
                        double res = op1 + op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
                        stack[stack_depth].as_float = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_FLOAT, 
                              TINYTEMPLATE_TYPE_FLOAT):
                    {
                        double op1 = stack[stack_depth-2].as_float;
                        double op2 = stack[stack_depth-1].as_float;
                        double res = op1 + op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
                        stack[stack_depth].as_float = res;
                        stack_depth++;
                        break;
                    }

                    default:
                    report(&error, "Addition on non-numeric operands");
                    return ETYPE;
                }
                break;
            }

            case OPCODE_SUB:
            {
                assert(stack_depth >= 2);

                switch PAIR(types[stack_depth-2], types[stack_depth-1]) {
                
                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_INT): 
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = stack[stack_depth-1].as_int;
                        int64_t res = op1 - op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_FLOAT):
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = (int64_t) stack[stack_depth-1].as_float;
                        int64_t res = op1 - op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_FLOAT, 
                              TINYTEMPLATE_TYPE_INT):
                    {
                        double op1 = stack[stack_depth-2].as_float;
                        double op2 = (double) stack[stack_depth-1].as_int;
                        double res = op1 - op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
                        stack[stack_depth].as_float = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_FLOAT, 
                              TINYTEMPLATE_TYPE_FLOAT):
                    {
                        double op1 = stack[stack_depth-2].as_float;
                        double op2 = stack[stack_depth-1].as_float;
                        double res = op1 - op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
                        stack[stack_depth].as_float = res;
                        stack_depth++;
                        break;
                    }

                    default:
                    report(&error, "Subtraction on non-numeric operands");
                    return ETYPE;
                }
                break;
            }

            case OPCODE_MUL:
            {
                assert(stack_depth >= 2);

                switch PAIR(types[stack_depth-2], types[stack_depth-1]) {
                
                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_INT): 
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = stack[stack_depth-1].as_int;
                        int64_t res = op1 * op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_FLOAT):
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = (int64_t) stack[stack_depth-1].as_float;
                        int64_t res = op1 * op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_FLOAT, 
                              TINYTEMPLATE_TYPE_INT):
                    {
                        double op1 = stack[stack_depth-2].as_float;
                        double op2 = (double) stack[stack_depth-1].as_int;
                        double res = op1 * op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
                        stack[stack_depth].as_float = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_FLOAT, 
                              TINYTEMPLATE_TYPE_FLOAT):
                    {
                        double op1 = stack[stack_depth-2].as_float;
                        double op2 = stack[stack_depth-1].as_float;
                        double res = op1 * op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
                        stack[stack_depth].as_float = res;
                        stack_depth++;
                        break;
                    }

                    default:
                    report(&error, "Multiplication on non-numeric operands");
                    return ETYPE;
                }
                break;
            }

            case OPCODE_DIV:
            {
                assert(stack_depth >= 2);

                switch PAIR(types[stack_depth-2], types[stack_depth-1]) {
                
                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_INT): 
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = stack[stack_depth-1].as_int;
                        int64_t res = op1 / op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_FLOAT):
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = (int64_t) stack[stack_depth-1].as_float;
                        int64_t res = op1 / op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_FLOAT,  
                              TINYTEMPLATE_TYPE_INT):
                    {
                        double op1 = stack[stack_depth-2].as_float;
                        double op2 = (double) stack[stack_depth-1].as_int;
                        double res = op1 / op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
                        stack[stack_depth].as_float = res;
                        stack_depth++;
                        break;
                    }

                    case PAIR(TINYTEMPLATE_TYPE_FLOAT, 
                              TINYTEMPLATE_TYPE_FLOAT):
                    {
                        double op1 = stack[stack_depth-2].as_float;
                        double op2 = stack[stack_depth-1].as_float;
                        double res = op1 / op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_FLOAT;
                        stack[stack_depth].as_float = res;
                        stack_depth++;
                        break;
                    }

                    default:
                    report(&error, "Division on non-numeric operands");
                    return ETYPE;
                }
                break;
            }

            case OPCODE_MOD:
            {
                assert(stack_depth >= 2);

                switch PAIR(types[stack_depth-2], types[stack_depth-1]) {
                
                    case PAIR(TINYTEMPLATE_TYPE_INT, 
                              TINYTEMPLATE_TYPE_INT): 
                    {
                        int64_t op1 = stack[stack_depth-2].as_int;
                        int64_t op2 = stack[stack_depth-1].as_int;
                        int64_t res = op1 % op2;
                        stack_depth -= 2;
                        types[stack_depth] = TINYTEMPLATE_TYPE_INT;
                        stack[stack_depth].as_int = res;
                        stack_depth++;
                        break;
                    }

                    default:
                    report(&error, "Modulo operator [mod] only works on integer operands");
                    return ETYPE;
                }
                break;
            }
        }
    }
    return DONE;
}

void tinytemplate_set_int(tinytemplate_value_t *dst, int64_t value)
{
    dst->type = TINYTEMPLATE_TYPE_INT;
    dst->data.as_int = value;
}

void tinytemplate_set_float(tinytemplate_value_t *dst, float value)
{
    dst->type = TINYTEMPLATE_TYPE_FLOAT;
    dst->data.as_float = value;
}

void tinytemplate_set_string(tinytemplate_value_t *dst, const char *str, size_t len)
{
    dst->type = TINYTEMPLATE_TYPE_STRING;
    dst->data.as_string.str = str;
    dst->data.as_string.len = len;
}

void tinytemplate_set_array(tinytemplate_value_t *dst, void *data, tinytemplate_nextcallback_t next)
{
    dst->type = TINYTEMPLATE_TYPE_ARRAY;
    dst->data.as_array.data = data;
    dst->data.as_array.next = next;
}

void tinytemplate_set_dict(tinytemplate_value_t *dst, void *data, tinytemplate_getter_t get)
{
    dst->type = TINYTEMPLATE_TYPE_DICT;
    dst->data.as_dict.data = data;
    dst->data.as_dict.get  = get;
}