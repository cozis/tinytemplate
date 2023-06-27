#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "xjson.h"
#include "tinytemplate.h"

typedef struct wrap_t wrap_t;
struct wrap_t {
    eval_context_t *context;
    union {
        xj_value *value;
        wrap_t *next;
    };
};

typedef struct {
    FILE     *stream;
    xj_value *params;
    wrap_t *free;
    wrap_t  pool[TINYTEMPLATE_MAX_ITER_DEPTH];
} eval_context_t;

static bool query_json_array(void *data,
                             tinytemplate_type_t *type, 
                             tinytemplate_value_t *value);

static bool query_json_object(void *data, const char *key, size_t len,
                             tinytemplate_type_t *type, 
                             tinytemplate_value_t *value);

static void convert_json_object(eval_context_t *context, 
                                xj_value *child, 
                                tinytemplate_type_t *type, 
                                tinytemplate_value_t *value)
{
    switch (child->type) {

        case XJ_NULL:
        *type = TINYTEMPLATE_TYPE_INT;
        value->as_int = 0;
        break;
        
        case XJ_INT:
        *type = TINYTEMPLATE_TYPE_INT;
        value->as_int = child->as_int;
        break;
        
        case XJ_FLOAT:
        *type = TINYTEMPLATE_TYPE_INT;
        value->as_float = child->as_float;
        break;
        
        case XJ_BOOL:
        *type = TINYTEMPLATE_TYPE_INT;
        value->as_int = child->as_bool;
        break;

        case XJ_ARRAY:
        {
            iter_t *iter = context->free;
            *type = TINYTEMPLATE_TYPE_ARRAY;
            value->as_array.data = iter;
            value->as_array.next  = query_json_array;
            if (iter) {
                context->free = iter->next;
                iter->curs = child->as_array;
            }
            break;
        }

        case XJ_OBJECT:
        *type = TINYTEMPLATE_TYPE_DICT;
        value->as_dict.data = child;
        value->as_dict.get  = query_json_object;
        break;

        case XJ_STRING: 
        *type = TINYTEMPLATE_TYPE_STRING;
        value->as_string.str = child->as_string;
        value->as_string.len = child->size;
        break;
    }
}

static bool query_json_array(void *data, 
                             tinytemplate_type_t *type, 
                             tinytemplate_value_t *value)
{
    iter_t *iter = data;
    
    if (iter->curs) {
        iter->curs = iter->curs->next;
        if (iter->curs) {
            convert_json_object(iter->curs, type, value);
            return true;
        }
    }
    return false;
}

static bool query_json_object(void *data, const char *key, size_t len,
                             tinytemplate_type_t *type, 
                             tinytemplate_value_t *value)
{
    xj_value *json_value = data;
    assert(json_value->type == XJ_OBJECT);

    xj_value *child = json_value->as_object;
    while (child) {
        size_t keylen = strlen(child->key);
        if (keylen == len && !strncmp(key, child->key, len))
            break;
        child = child->next;
    }
    if (!child)
        return false;
    
    convert_json_object(child, type, value);
    return true;
}

static void callback(void *userp, const char *str, size_t len)
{
    eval_context_t *context = userp;
    fwrite(str, 1, len, context->stream);
}

static bool query_root_json_object(void *data, const char *key, size_t len,
                                   tinytemplate_type_t *type, 
                                   tinytemplate_value_t *value)
{
    eval_context_t *context = data;
    if (context->params)
        return query_json_object(context->params, key, len, type, value);
    else
        return false;
}

static void init_context(eval_context_t *context,
                         FILE *stream, xj_value *root)
{
    context->stream=stdout; 
    context->params=root;
    context->free = context->pool;

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
    init_context(&context);

    if (tinytemplate_eval(buffer, program, &context, query_root_json_object, callback, message, sizeof(message)) != TINYTEMPLATE_STATUS_DONE) {
        fprintf(stderr, "Failed to evaluate template (%s)\n", message);
        free(program);
        return -1;
    }

    free(program);
    return 0;
}
