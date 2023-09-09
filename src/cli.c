#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "xjson.h"
#include "tinytemplate.h"

typedef struct wrap_t wrap_t;
typedef struct eval_context_t eval_context_t;

struct wrap_t {
    eval_context_t *context;
    union {
        xj_value *value;
        wrap_t   *next;
    };
};

struct eval_context_t {
    FILE     *stream;
    wrap_t    params;
    wrap_t   *free;
    wrap_t    pool[TINYTEMPLATE_MAX_ITER_DEPTH];
};

static bool query_json_array(void *data, tinytemplate_value_t *value);

static bool query_json_object(void *data, const char *key, size_t len,
                              tinytemplate_value_t *value);

static void convert_json_object(eval_context_t *context, xj_value *child, 
                                tinytemplate_value_t *value)
{
    switch (child->type) {

        case XJ_NULL : tinytemplate_set_int(value, 0); break;
        case XJ_INT  : tinytemplate_set_int(value, child->as_int); break;
        case XJ_FLOAT: tinytemplate_set_float(value, child->as_float); break;
        case XJ_BOOL : tinytemplate_set_int(value, child->as_bool); break;
        case XJ_OBJECT: tinytemplate_set_dict(value, child, query_json_object); break;
        case XJ_STRING: tinytemplate_set_string(value, child->as_string, child->size); break;

        case XJ_ARRAY:
        {
            wrap_t *wrap = context->free;
            tinytemplate_set_array(value, wrap, query_json_array);
            if (wrap) {
                context->free = wrap->next;
                wrap->value = child->as_array;
                wrap->context = context;
            }
            break;
        }
    }
}

static bool query_json_array(void *data, tinytemplate_value_t *value)
{
    wrap_t *wrap = data;

    if (wrap->value) {
        convert_json_object(wrap->context, wrap->value, value);
        wrap->value = wrap->value->next;
        return true;
    }
    return false;
}

static bool query_json_object(void *data, const char *key, size_t len,
                              tinytemplate_value_t *value)
{
    wrap_t *wrap = data;
    
    assert(wrap->value->type == XJ_OBJECT);

    xj_value *child = wrap->value->as_object;
    while (child) {
        size_t keylen = strlen(child->key);
        if (keylen == len && !strncmp(key, child->key, len))
            break;
        child = child->next;
    }
    if (!child)
        return false;
    
    convert_json_object(wrap->context, child, value);
    return true;
}

static void callback(void *userp, const char *str, size_t len)
{
    eval_context_t *context = userp;
    fwrite(str, 1, len, context->stream);
}

static bool query_root_json_object(void *data, const char *key, size_t len,
                                   tinytemplate_value_t *value)
{
    eval_context_t *context = data;
    if (context->params.value)
        return query_json_object(&context->params, key, len, value);
    else
        return false;
}

static void init_context(eval_context_t *context,
                         FILE *stream, xj_value *root)
{
    context->stream = stream;
    context->params.context = context;
    context->params.value = root;
    context->free   = context->pool;

    for (int i = 0; i < TINYTEMPLATE_MAX_ITER_DEPTH-1; i++)
        context->pool[i].next = &context->pool[i+1];
    context->pool[TINYTEMPLATE_MAX_ITER_DEPTH-1].next = NULL;
}

int main(int argc, char **argv)
{
    const char *param_file = NULL;
    if (argc > 1)
        param_file = argv[1];

    char buffer[1 << 16];
    char   json[1 << 16];
    xj_value *root = NULL;
    xj_alloc *alloc = NULL;

    if (param_file) {
        
        FILE *stream = fopen(param_file, "rb");
        if (stream == NULL) {
            fprintf(stderr, "Couldn't open parameter file \"%s\"\n", param_file);
            return -1;
        }
        
        size_t num = fread(buffer, 1, sizeof(buffer), stream);
        if (ferror(stream)) {
            fprintf(stderr, "Couldn't read from \"%s\"\n", param_file);
            fclose(stream);
            return -1;
        }

        alloc = xj_alloc_using(json, sizeof(json), 0, NULL);

        xj_error error;
        root = xj_decode(buffer, (int) num, alloc, &error);
        if (root == NULL) {
            fprintf(stderr, "Couldn't decode JSON (%s)\n", error.message);
            fclose(stream);
            return -1;
        }

        fclose(stream);
    }
    
    size_t num = fread(buffer, 1, sizeof(buffer), stdin);
    if (ferror(stdin)) {
        fprintf(stderr, "Failed to read from stdin\n");
        return -1;
    }
    
    char message[256];

    tinytemplate_instr_t *program = NULL;
    size_t max_instr = 1 << 10;

    tinytemplate_status_t status;
    do {
        max_instr *= 2;
        program = realloc(program, max_instr * sizeof(tinytemplate_instr_t));
        if (program == NULL) {
            fprintf(stderr, "Out of memory");
            return -1;
        }

        status = tinytemplate_compile(buffer, num, program, max_instr, NULL, message, sizeof(message));
        if (status != TINYTEMPLATE_STATUS_DONE &&
            status != TINYTEMPLATE_STATUS_EMEMORY) {
            fprintf(stderr, "Failed to compile template (%s)\n", message);
            return -1;
        }
    } while (status == TINYTEMPLATE_STATUS_EMEMORY);

    eval_context_t context;
    init_context(&context, stdout, root);

    if (tinytemplate_eval(buffer, program, &context, query_root_json_object, callback, message, sizeof(message)) != TINYTEMPLATE_STATUS_DONE) {
        fprintf(stderr, "Failed to evaluate template (%s)\n", message);
        free(program);
        return -1;
    }

    free(program);
    return 0;
}
