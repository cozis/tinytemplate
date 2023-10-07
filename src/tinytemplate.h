#ifndef TINYTEMPLATE_H
#define TINYTEMPLATE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifndef TINYTEMPLATE_MAX_SCOPE_DEPTH
#define TINYTEMPLATE_MAX_SCOPE_DEPTH 8
#endif

#ifndef TINYTEMPLATE_MAX_EXPR_DEPTH
#define TINYTEMPLATE_MAX_EXPR_DEPTH 8
#endif

#ifndef TINYTEMPLATE_MAX_ITER_DEPTH
#define TINYTEMPLATE_MAX_ITER_DEPTH 8
#endif

typedef enum {
    TINYTEMPLATE_TYPE_INT,
    TINYTEMPLATE_TYPE_FLOAT,
    TINYTEMPLATE_TYPE_DICT,
    TINYTEMPLATE_TYPE_ARRAY,
    TINYTEMPLATE_TYPE_STRING,
} tinytemplate_type_t;

typedef struct tinytemplate_value_t tinytemplate_value_t;

typedef bool (*tinytemplate_nextcallback_t)(void*, tinytemplate_value_t*);
typedef bool (*tinytemplate_getter_t)(void*, const char*, size_t, tinytemplate_value_t*);
typedef void (*tinytemplate_callback_t)(void *userp, const char *str, size_t len);

typedef struct {
    void *data;
    tinytemplate_getter_t get;
} tinytemplate_dict_t;

typedef struct {
    void *data;
    tinytemplate_nextcallback_t next;
} tinytemplate_array_t;

typedef struct {
    const char *str; size_t len;
} tinytemplate_string_t;

typedef union {
    int64_t as_int;
    double  as_float;
    tinytemplate_dict_t   as_dict;
    tinytemplate_array_t  as_array;
    tinytemplate_string_t as_string;
} tinytemplate_union_t;

struct tinytemplate_value_t {
    tinytemplate_type_t  type;
    tinytemplate_union_t data;
};

typedef struct {
    int opcode;
    struct {
        int64_t as_int;
        size_t  as_size;
        double  as_float;
    } operands[2];
} tinytemplate_instr_t;

typedef enum {
    TINYTEMPLATE_STATUS_DONE,
    TINYTEMPLATE_STATUS_ESYMBOL,
    TINYTEMPLATE_STATUS_ESCOPE,
    TINYTEMPLATE_STATUS_EDEPTH,
    TINYTEMPLATE_STATUS_ETYPE,
    TINYTEMPLATE_STATUS_EITER,
    TINYTEMPLATE_STATUS_EMEMORY,
    TINYTEMPLATE_STATUS_ESYNTAX,
    TINYTEMPLATE_STATUS_ESEMANT,
} tinytemplate_status_t;

tinytemplate_status_t 
tinytemplate_eval(const char *src, const tinytemplate_instr_t *program, 
                  void *userp, tinytemplate_getter_t params,
                  tinytemplate_callback_t callback,
                  char *errmsg, size_t errmax);

tinytemplate_status_t 
tinytemplate_compile(const char *src, size_t len, 
                     tinytemplate_instr_t *program,
                     size_t max_instr, size_t *num_instr,
                     char *errmsg, size_t errmax);

void tinytemplate_set_int(tinytemplate_value_t *dst, int64_t value);
void tinytemplate_set_float(tinytemplate_value_t *dst, float value);
void tinytemplate_set_string(tinytemplate_value_t *dst, const char *str, size_t len);
void tinytemplate_set_array(tinytemplate_value_t *dst, void *data, tinytemplate_nextcallback_t next);
void tinytemplate_set_dict(tinytemplate_value_t *dst, void *data, tinytemplate_getter_t get);

#endif
